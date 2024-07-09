// rustc crates
use rustc_ast::ast::LitKind;
use rustc_hir::Lit;
use rustc_middle::mir::{BinOp, BorrowKind, UnOp};
use rustc_middle::thir::LocalVarId;
use rustc_middle::thir::LogicalOp;
use rustc_middle::ty::TyKind;
use rustc_span::{def_id::LocalDefId, Span};

// std crates
use std::collections::{HashMap as Map, VecDeque};
use std::io::Write;
use std::process::Command;
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;
use regex::Regex;
mod lir;
use lir::*;

pub fn analyze<'tcx>(
    main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
) -> Result<(), AnalysisError> {
    Analyzer::run(main_id, fn_map)
}

#[derive(Debug)]
struct Analyzer<'tcx> {
    fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    path_map: Map<(Lir<'tcx>, bool), VecDeque<Lir<'tcx>>>,
    current_path: VecDeque<Lir<'tcx>>,
    var_map: Map<LocalVarId, String>,
}

#[derive(Debug)]
pub enum AnalysisError {
    FunctionNotFound(LocalDefId),
    UnsupportedPattern(String),
    RandFunctions,
    VerifyError { message: String, span: Span },
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>) -> Self {
        Self { fn_map, path_map: Map::new(), current_path: VecDeque::new(), var_map: Map::new() }
    }

    pub fn run(
        main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    ) -> Result<(), AnalysisError> {
        let mut analyzer = Analyzer::new(fn_map);
        let main = analyzer.get_fn(main_id)?;
        match analyzer.analyze_fn(main, &[]) {
            Ok(_) => Ok(()),
            Err(why) => Err(why),
        }
    }

    fn verify(&self) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .arg("-in")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut stdin = child.stdin.take().expect("Open std failed");
        let smt = self.get_current_assumptions()?;
        println!("{}", smt);
        stdin.write_all(smt.as_bytes()).expect("Write smt failed");
        drop(stdin);

        let output = child.wait_with_output().expect("Get stdout failed");
        let result = String::from_utf8(output.stdout).expect("Load result failed");
        if &result != "unsat\n" {
            return Err(AnalysisError::VerifyError {
                message: result,
                span: self.get_current_span(),
            });
        }

        println!("Verification success!\n");

        Ok(())
    }

    fn get_current_assumptions(&self) -> Result<String, AnalysisError> {
        let mut smt = String::new();
        let len = self.current_path.len();
        let mut cnt = 0;
        loop {
            // println!("{:?}", self.current_path[cnt]);
            if len - 1 == cnt {
                smt.push_str(&self.current_path[cnt].to_assert());
                break;
            }
            smt.push_str(&self.current_path[cnt].to_smt()?);
            cnt += 1;
        }
        smt += "(check-sat)";
        Ok(smt)
    }

    fn get_current_span(&self) -> Span {
        self.current_path.back().expect("Lir not found").get_span()
    }

    fn get_fn(&self, fn_id: LocalDefId) -> Result<Rc<RThir<'tcx>>, AnalysisError> {
        self.fn_map.get(&fn_id).cloned().ok_or(AnalysisError::FunctionNotFound(fn_id))
    }

    fn analyze_fn(
        &mut self, rthir: Rc<RThir<'tcx>>, args: &[Rc<RExpr<'tcx>>],
    ) -> Result<Option<String>, AnalysisError> {
        let mut return_value = None;
        self.analyze_params(&rthir.params, args)?;
        if let Some(body) = &rthir.body {
            return_value = self.analyze_body((*body).clone())?;
        }
        Ok(return_value)
    }

    fn analyze_params(
        &mut self, params: &[RParam<'tcx>], args: &[Rc<RExpr<'tcx>>],
    ) -> Result<(), AnalysisError> {
        use RExprKind::*;
        use RPatKind::*;

        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pat) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = &**pat {
                    match kind {
                        Binding { name, ty, var, .. } => {
                            let parameter =
                                Lir::new_parameter(name.clone(), ty.clone(), pat.clone());
                            self.current_path.push_back(parameter);
                            self.var_map.insert(var.clone(), format!("{}", name));
                            let new_assume = Lir::new_assume(
                                format!("(= {} {})", name, self.expr_to_string(arg.clone())?),
                                arg.clone(),
                            );
                            self.current_path.push_back(new_assume);
                        }
                        Wild => (),
                        _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
                    }
                }
            }
        }
        Ok(())
    }

    fn expr_to_string(&mut self, arg: Rc<RExpr<'tcx>>) -> Result<String, AnalysisError> {
        use RExprKind::*;

        match &arg.kind {
            VarRef { id } => Ok(self.get_var(*id)),
            Literal { lit, neg } => Analyzer::literal_to_string(lit, *neg),
            LogicalOp { op, lhs, rhs } => {
                let lhs_str = self.expr_to_string(lhs.clone())?;
                let rhs_str = self.expr_to_string(rhs.clone())?;
                self.logical_op_to_string(*op, lhs_str, rhs_str)
            }
            Binary { op, lhs, rhs } => {
                let lhs_str = self.expr_to_string(lhs.clone())?;
                let rhs_str = self.expr_to_string(rhs.clone())?;
                self.bin_op_to_string(*op, lhs_str, rhs_str)
            }
            Call { ty, args, .. } => match ty.kind() {
                TyKind::FnDef(def_id, ..) => {
                    if def_id.is_local() {
                        if let Some(fun) = self.fn_map.get(&def_id.expect_local()) {
                            match self.analyze_fn(fun.clone(), &**args) {
                                Ok(some) => Ok(some.unwrap()),
                                Err(why) => Err(why),
                            }
                        } else {
                            unreachable!("Function is not local");
                        }
                    } else {
                        let re = Regex::new(r"DefId\(\d+:\d+ ~ ([\w]+)\[[\w]+\]::([\w]+)").unwrap();
                        let fn_name = format!("{:?}", def_id);
                        if let Some(captures) = re.captures(&fn_name) {
                            if &captures[1] == "t3modules" {
                                match &captures[2] {
                                    "rand_bool" => Err(AnalysisError::RandFunctions),
                                    "rand_int" => Err(AnalysisError::RandFunctions),
                                    "rand_float" => Err(AnalysisError::RandFunctions),
                                    _ => unreachable!(),
                                }
                            } else {
                                panic!("Unknown module");
                            }
                        } else {
                            panic!("Unknown module");
                        }
                    }
                }
                _ => panic!("Call has not have FnDef"),
            },
            _ => {
                println!("{}", self.get_current_assumptions()?);
                Err(AnalysisError::UnsupportedPattern(format!("name: {:?}", arg.kind)))
            }
        }
    }

    fn get_var(&self, var_id: LocalVarId) -> String { self.var_map.get(&var_id).unwrap().clone() }

    fn literal_to_string(lit: &'tcx Lit, neg: bool) -> Result<String, AnalysisError> {
        match lit.node {
            LitKind::Str(symbol, _) => Ok(symbol.to_string()),
            LitKind::Char(c) => Ok(format!("'{}'", c)),
            LitKind::Int(n, _) => Ok(if neg { format!("-{}", n) } else { format!("{}", n) }),
            LitKind::Float(symbol, _) => {
                Ok(if neg { format!("-{}", symbol) } else { format!("{}", symbol) })
            }
            LitKind::Bool(b) => Ok(b.to_string()),
            LitKind::ByteStr(ref bytes, _) => {
                Ok(format!("b\"{}\"", String::from_utf8_lossy(bytes)))
            }
            _ => Err(AnalysisError::UnsupportedPattern(format!(
                "Unsupported literal pattern: {}",
                lit.node
            ))),
        }
    }

    fn logical_op_to_string(
        &self, op: LogicalOp, lhs: String, rhs: String,
    ) -> Result<String, AnalysisError> {
        use LogicalOp::*;

        let op_str = match op {
            And => "and",
            Or => "or",
        };
        Ok(format!("({} {} {})", op_str, lhs, rhs))
    }

    fn bin_op_to_string(
        &self, op: BinOp, lhs: String, rhs: String,
    ) -> Result<String, AnalysisError> {
        use BinOp::*;

        let op_str = match op {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Rem => "%",
            Div => "/",
            BitXor => "^",
            BitAnd => "&",
            BitOr => "|",
            Eq => "=",
            Lt => "<",
            Le => "<=",
            Ne => "!=",
            Ge => ">=",
            Gt => ">",
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", op))),
        };
        Ok(format!("({} {} {})", op_str, lhs, rhs))
    }

    fn analyze_pat(
        &mut self, kind: &RPatKind<'tcx>, pat: Rc<RExpr<'tcx>>,
    ) -> Result<(), AnalysisError> {
        use RPatKind::*;

        match kind {
            Wild => (),
            Binding { name, ty, var, .. } => {
                let parameter = Lir::<'tcx>::new_parameter(name.clone(), ty.clone(), pat.clone());
                self.current_path.push_back(parameter);
                self.var_map.insert(var.clone(), format!("{}", name));
            }
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    fn analyze_body(&mut self, body: Rc<RExpr<'tcx>>) -> Result<Option<String>, AnalysisError> {
        let mut return_value = None;
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*body {
            for stmt in stmts {
                if let Some(value) = self.analyze_expr(stmt.clone())? {
                    return_value = Some(value);
                }
            }
            if let Some(expr) = expr {
                self.analyze_expr(expr.clone())?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(return_value)
    }

    fn analyze_expr(&mut self, expr: Rc<RExpr<'tcx>>) -> Result<Option<String>, AnalysisError> {
        use RExprKind::*;

        let kind = &expr.kind;
        let expr = expr.clone();

        match kind {
            Pat { kind } => self.analyze_pat(&kind, expr)?,
            Call { ty, args, .. } => match ty.kind() {
                TyKind::FnDef(def_id, ..) => {
                    if def_id.is_local() {
                        if let Some(fun) = self.fn_map.get(&def_id.expect_local()) {
                            // self.current_path.push_back(Lir::new_push(expr));
                            self.analyze_fn(fun.clone(), &**args)?;
                        }
                    } else {
                        let re = Regex::new(r"DefId\(\d+:\d+ ~ ([\w]+)\[[\w]+\]::([\w]+)").unwrap();
                        let fn_name = format!("{:?}", def_id);
                        if let Some(captures) = re.captures(&fn_name) {
                            if &captures[1] == "t3modules" {
                                match &captures[2] {
                                    "t3assert" => self.analyze_t3assert(&**args)?,
                                    "t3assume" => self.analyze_t3assume(&**args)?,
                                    _ => unreachable!(),
                                }
                            } else {
                                panic!("Unknown module");
                            }
                        }
                    }
                }
                _ => panic!("Call has not have FnDef"),
            },
            LetStmt { pattern, initializer, else_block: _ } => {
                self.analyze_let_stmt(pattern, initializer)?
            }
            Return { value } => {
                if let Some(expr) = value {
                    return Ok(Some(self.expr_to_string(expr.clone())?));
                }
            }
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", expr.kind))),
        }
        Ok(None)
    }

    fn analyze_t3assert(&mut self, args: &[Rc<RExpr<'tcx>>]) -> Result<(), AnalysisError> {
        self.analyze_t3assume(args)?;
        self.verify()
    }

    fn analyze_t3assume(&mut self, args: &[Rc<RExpr<'tcx>>]) -> Result<(), AnalysisError> {
        let new_assume = Lir::new_assume(self.expr_to_string(args[0].clone())?, args[0].clone());
        self.current_path.push_back(new_assume);
        Ok(())
    }

    fn analyze_let_stmt(
        &mut self, pattern: &Rc<RExpr<'tcx>>, initializer: &Option<Rc<RExpr<'tcx>>>,
    ) -> Result<(), AnalysisError> {
        if let RExprKind::Pat { kind: RPatKind::Binding { name, ty, var, .. } } = pattern.kind {
            let declaration = Lir::new_parameter(name.clone(), ty.clone(), pattern.clone());
            self.current_path.push_back(declaration);
            self.var_map.insert(var.clone(), format!("{}", name));
            if let Some(init) = initializer {
                match self.expr_to_string(init.clone()) {
                    Ok(string) => {
                        self.current_path.push_back(Lir::new_assume(
                            format!("(= {} {})", name, string),
                            init.clone(),
                        ));
                    }
                    Err(err) => match err {
                        AnalysisError::RandFunctions => {}
                        _ => return Err(err),
                    },
                }
            }
        } else {
            unreachable!();
        }
        Ok(())
    }
}
