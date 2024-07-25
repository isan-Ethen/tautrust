// rustc crates
use rustc_ast::ast::LitKind;
use rustc_hir::Lit;
use rustc_middle::mir::{BinOp, UnOp};
use rustc_middle::thir::LocalVarId;
use rustc_middle::thir::LogicalOp;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::{Ty, TyKind};
use rustc_span::{
    def_id::{DefId, LocalDefId},
    Span,
};

// std crates
use std::boxed::Box;
use std::collections::HashMap as Map;
use std::io::Write;
use std::process::Command;
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;
mod lir;
pub use lir::*;
mod env;
use env::Env;

pub fn analyze<'tcx>(
    main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>,
) -> Result<(), AnalysisError> {
    Analyzer::run(main_id, fn_map, tcx)
}

enum AnalysisType<'tcx> {
    Return(Option<String>),
    Invariant(Rc<RExpr<'tcx>>),
    Other,
}

#[derive(Debug)]
pub enum AnalysisError {
    FunctionNotFound(LocalDefId),
    UnsupportedPattern(String),
    RandFunctions,
    VerifyError { span: Span },
}
struct Analyzer<'tcx> {
    fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>) -> Self {
        Self { fn_map, tcx }
    }

    pub fn run(
        main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut analyzer = Analyzer::new(fn_map, tcx);
        let main = analyzer.get_fn(main_id)?;
        analyzer.analyze_main(main)
    }

    /// Utility functions
    /// - verify
    /// - get_current_assumptions_for_verify
    /// - get_current_assumptions
    /// - get_current_span
    /// - get_fn
    /// - add_assumption
    /// - add_parameter
    /// - get_var
    /// - get_local_fn
    /// - get_fn_info
    /// - assign_new_value
    /// - new_env_name

    fn verify(&self, mut smt: String, env: &Env<'tcx>) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .args(["-in", "-model"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut stdin = child.stdin.take().expect("Open std failed");
        smt += "(check-sat)\n";
        println!("{}", smt);
        stdin.write_all(smt.as_bytes()).expect("Write smt failed");
        drop(stdin);

        let output = child.wait_with_output().expect("Get stdout failed");
        let result = String::from_utf8(output.stdout).expect("Load result failed");
        if &result != "unsat\n" {
            return Err(AnalysisError::VerifyError { span: env.get_latest_span() });
        }

        println!("Verification success!\n");

        Ok(())
    }

    fn get_fn(&self, fn_id: LocalDefId) -> Result<Rc<RThir<'tcx>>, AnalysisError> {
        self.fn_map.get(&fn_id).cloned().ok_or(AnalysisError::FunctionNotFound(fn_id))
    }

    fn get_local_fn(&self, def_id: &DefId) -> Option<Rc<RThir<'tcx>>> {
        if def_id.is_local() {
            Some(self.fn_map.get(&def_id.expect_local()).expect("Get local fn failed").clone())
        } else {
            None
        }
    }

    fn get_fn_info(&self, def_id: &DefId) -> Vec<String> {
        let def_path = self.tcx.def_path_str(*def_id);
        def_path
            .split(|c| c == ':' || c == '"' || c == '\\')
            .filter(|s| !s.is_empty())
            .map(String::from)
            .collect()
    }

    /// Main analysis functions
    /// - analyze_main
    /// - analyze_params
    /// - analyze_body
    /// - analyze_expr
    /// - analyze_loop

    fn analyze_main(&mut self, rthir: Rc<RThir<'tcx>>) -> Result<(), AnalysisError> {
        if let Some(body) = &rthir.body {
            let mut main_env = Env::new();
            self.analyze_body((*body).clone(), &mut main_env)?;
        }
        Ok(())
    }

    fn analyze_params(
        &mut self, params: &Vec<RParam<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RExprKind::*;
        use RPatKind::*;

        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pat) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = &**pat {
                    match kind {
                        Binding { name, ty, var, .. } => {
                            let env_name = format!("{}_{}", env.name, name);
                            env.add_parameter(env_name.clone(), ty, var, pat.clone());
                            let value = self.expr_to_constraint(arg.clone(), env)?;
                            env.add_assumption(format!("(= {} {})", env_name, value), arg.clone());
                        }
                        Wild => (),
                        _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
                    }
                }
            }
        }
        Ok(())
    }

    fn analyze_body(
        &mut self, body: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*body {
            let mut stmts_iter = stmts.iter();
            while let Some(stmt) = stmts_iter.next() {
                let return_value = self.analyze_expr(stmt.clone(), env)?;
                match &return_value {
                    AnalysisType::Return(..) => break,
                    AnalysisType::Invariant(expr) => self.analyze_loop(
                        expr.clone(),
                        stmts_iter.next().expect("No loop expression found").clone(),
                        env,
                    )?,
                    AnalysisType::Other => (),
                }
            }
            if let Some(expr) = expr {
                self.analyze_expr(expr.clone(), env)?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(())
    }

    fn analyze_expr(
        &mut self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        use RExprKind::*;

        let mut return_value = AnalysisType::Other;

        match expr.kind.clone() {
            Literal { .. } => self.analyze_literal(expr, env)?,
            VarRef { .. } => self.analyze_var_ref(expr, env)?,
            Binary { .. } => self.analyze_binary(expr, env)?,
            Pat { kind } => self.analyze_pat(&kind, expr, env)?,
            Call { ty, args, .. } => return_value = self.analyze_fn(ty, args, expr, env)?,
            LetStmt { pattern, initializer, else_block } => {
                self.analyze_let_stmt(pattern, initializer, else_block, env)?
            }
            Return { value } => {
                if let Some(expr) = value {
                    return_value = AnalysisType::Return(Some(self.expr_to_constraint(expr, env)?));
                } else {
                    return_value = AnalysisType::Return(None);
                }
            }
            AssignOp { op, lhs, rhs } => self.analyze_assign_op(op, lhs, rhs, expr, env)?,
            Assign { lhs, rhs } => self.analyze_assign(lhs, rhs, expr, env)?,
            If { cond, then, else_opt } => self.analyze_if(cond, then, else_opt, env)?,
            // Break { .. } => (),
            _ => {
                println!("{:?}", expr.kind);
                return Err(AnalysisError::UnsupportedPattern("Unknown expr".into()));
            }
        }
        Ok(return_value)
    }

    fn analyze_loop(
        &mut self, invariant: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut loop_env = env.gen_new_env("loop".to_string(), expr.clone())?;
        let constraint = self.expr_to_constraint(invariant.clone(), &mut loop_env)?;
        self.verify_before_loop(&constraint, env)?;
        if let RExprKind::Loop { body } = expr.kind.clone() {
            loop_env.gen_new_env("inner_loop".to_string(), expr.clone())?;
            self.verify_inner_loop(constraint, invariant, body.clone(), env)?;
        } else {
            return Err(AnalysisError::UnsupportedPattern(
                "Multiple invariant is not suppoerted".into(),
            ));
        }
        Ok(())
    }

    fn verify_before_loop(
        &self, constraint: &String, env: &Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut smt = env.get_assumptions()?;
        smt += &format!("(assert (not {}))\n", constraint);
        self.verify(smt, env)
    }

    fn verify_inner_loop(
        &mut self, constraint: String, invariant: Rc<RExpr<'tcx>>, block: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.set_var_map(block.clone(), invariant.clone(), env);
        env.add_assumption(constraint, invariant.clone());
        let smt = env.get_assumptions_for_verify()?;
        println!("{}", smt);
        self.analyze_block(block, env)?;
        let smt = env.get_assumptions_for_verify()?;
        println!("{}", smt);
        self.verify(smt, env)
    }

    fn set_var_map(
        &mut self, block: Rc<RExpr<'tcx>>, invariant: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) {
        let inv_varv = Analyzer::search_inv(invariant);
        let varv = Analyzer::search_used_var(block.clone());
        let refresh_varv = varv.iter().filter(|var| !inv_varv.contains(var));
        for var in refresh_varv {
            let (current_name, ty) = env.var_map.get(var).unwrap().clone();
            let new_name = format!("{}_{}", env.name, current_name);
            env.add_parameter(new_name.clone(), &ty, var, block.clone());
            env.var_map.insert(*var, (new_name, ty.clone()));
        }
    }

    fn search_inv(invariant: Rc<RExpr<'tcx>>) -> Vec<LocalVarId> {
        let mut varv: Vec<LocalVarId> = Vec::new();
        Analyzer::search_var(invariant, &mut varv);
        varv
    }

    fn search_var(expr: Rc<RExpr<'tcx>>, varv: &mut Vec<LocalVarId>) {
        use RExprKind::*;

        match expr.kind.clone() {
            VarRef { id } => {
                varv.push(id.clone());
            }
            LogicalOp { lhs, rhs, .. } => {
                Analyzer::search_var(lhs.clone(), varv);
                Analyzer::search_var(rhs.clone(), varv);
            }
            Unary { arg, .. } => {
                Analyzer::search_var(arg.clone(), varv);
            }
            Binary { lhs, rhs, .. } => {
                Analyzer::search_var(lhs.clone(), varv);
                Analyzer::search_var(rhs.clone(), varv);
            }
            _ => panic!("Unknown invariant pattern"),
        }
    }

    fn search_used_var(block: Rc<RExpr<'tcx>>) -> Vec<LocalVarId> {
        let mut varv: Vec<LocalVarId> = Vec::new();
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*block {
            for stmt in stmts {
                Analyzer::search_var_expr(stmt.clone(), &mut varv, false);
            }
            if let Some(expr) = expr {
                Analyzer::search_var_expr(expr.clone(), &mut varv, false);
            }
        }
        varv
    }

    fn search_var_expr(expr: Rc<RExpr<'tcx>>, varv: &mut Vec<LocalVarId>, is_assign: bool) {
        use RExprKind::*;

        match &expr.kind {
            Literal { .. } => (),
            VarRef { id } => {
                if is_assign {
                    varv.push(id.clone());
                }
            }
            LogicalOp { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, is_assign);
                Analyzer::search_var_expr(rhs.clone(), varv, is_assign);
            }
            Unary { arg, .. } => {
                Analyzer::search_var_expr(arg.clone(), varv, is_assign);
            }
            Binary { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, is_assign);
                Analyzer::search_var_expr(rhs.clone(), varv, is_assign);
            }
            Call { .. } => (),
            If { then, else_opt, .. } => {
                Analyzer::search_var_expr(then.clone(), varv, is_assign);
                if let Some(else_block) = else_opt {
                    Analyzer::search_var_expr(else_block.clone(), varv, is_assign);
                }
            }
            LetStmt { initializer, .. } => {
                if let Some(initializer) = initializer {
                    Analyzer::search_var_expr(initializer.clone(), varv, is_assign);
                }
            }
            AssignOp { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, true);
                Analyzer::search_var_expr(rhs.clone(), varv, false);
            }
            Assign { lhs, rhs } => {
                Analyzer::search_var_expr(lhs.clone(), varv, true);
                Analyzer::search_var_expr(rhs.clone(), varv, false);
            }
            Block { stmts, expr } => {
                for stmt in stmts {
                    Analyzer::search_var_expr(stmt.clone(), varv, is_assign);
                }
                if let Some(expr) = expr {
                    Analyzer::search_var_expr(expr.clone(), varv, false);
                }
            }
            // Break { .. } => (),
            _ => panic!("Unknown pattern in loop: {:?}", expr),
        }
    }

    /// Sub analysis functions
    /// - analyze_local_fn
    /// - analyze_literal
    /// - analyze_var_ref
    /// - analyze_binary
    /// - analyze_pat
    /// - analyze_fn
    /// - analyze_extern_fn
    /// - analyze_let_stmt
    /// - analyze_assign_op
    /// - analyze_assign
    /// - analyze_if
    /// - analyze_block

    fn analyze_local_fn(
        &mut self, rthir: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.analyze_params(&rthir.params, args, env)?;
        if let Some(body) = &rthir.body {
            self.analyze_body((*body).clone(), env)?;
        }
        Ok(())
    }

    fn analyze_literal(
        &mut self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let constraint = self.expr_to_constraint(expr.clone(), env)?;
        env.add_assumption(constraint, expr);
        Ok(())
    }

    fn analyze_var_ref(
        &mut self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let constraint = self.expr_to_constraint(expr.clone(), env)?;
        env.add_assumption(constraint, expr);
        Ok(())
    }

    fn analyze_binary(
        &mut self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let constraint = self.expr_to_constraint(expr.clone(), env)?;
        env.add_assumption(constraint, expr);
        Ok(())
    }

    fn analyze_pat(
        &mut self, kind: &RPatKind<'tcx>, pat: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RPatKind::*;

        match kind {
            Wild => (),
            Binding { name, ty, var, .. } => env.add_parameter(name.to_string(), ty, var, pat),
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    fn analyze_fn(
        &mut self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, expr: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        match ty.kind() {
            TyKind::FnDef(def_id, ..) => {
                let mut fn_info = self.get_fn_info(def_id);
                if let Some(fun) = self.get_local_fn(def_id) {
                    let fn_env =
                        env.gen_new_env(fn_info.pop().expect("fn info not found"), expr)?;
                    match self.analyze_local_fn(fun, args, env) {
                        Ok(()) => {
                            env.merge_env(fn_env);
                            Ok(AnalysisType::Other)
                        }
                        Err(why) => Err(why),
                    }
                } else {
                    self.analyze_extern_fn(fn_info, args, env)
                }
            }
            _ => panic!("Call has not have FnDef"),
        }
    }

    fn analyze_extern_fn(
        &mut self, fn_info: Vec<String>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        if fn_info[0] == "t3modules" {
            match fn_info[1].as_str() {
                "t3assert" => self.analyze_t3assert(args, env),
                "t3assume" => self.analyze_t3assume(args, env),
                "invariant" => self.analyze_invariant(args),
                _ => unreachable!(),
            }
        } else {
            Err(AnalysisError::UnsupportedPattern("Unknown function!".into()))
        }
    }

    fn analyze_let_stmt(
        &mut self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>,
        _: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExprKind::Pat { kind: RPatKind::Binding { name, ty, var, .. } } = &pattern.kind {
            let name = format!("{}_{}", env.name, name);
            let declaration = Lir::new_parameter(name.clone(), ty.clone(), pattern.clone());
            env.add_lir(declaration);
            env.insert_var(var, name.clone(), ty);
            if let Some(init) = initializer {
                match self.expr_to_constraint(init.clone(), env) {
                    Ok(value) => {
                        env.add_assumption(format!("(= {} {})", name, value), init.clone())
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

    fn analyze_assign_op(
        &mut self, op: BinOp, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let rhs = self.expr_to_constraint(rhs, env)?;
        let (new_lhs, lhs) = env.assign_new_value(lhs.clone())?;
        let constraint = self.bin_op_to_constraint(op, &lhs, &rhs)?;
        env.add_assumption(format!("(= {} {})", new_lhs, constraint), expr);
        Ok(())
    }

    fn analyze_assign(
        &mut self, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let rhs = self.expr_to_constraint(rhs.clone(), env)?;
        let (new_lhs, _) = env.assign_new_value(lhs.clone())?;
        env.add_assumption(format!("(= {} {})", new_lhs, rhs), expr.clone());
        Ok(())
    }

    fn analyze_if(
        &mut self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let cond_str = self.expr_to_constraint(cond.clone(), env)?;

        let mut then_env = env.gen_new_env("then".to_string(), then_block.clone())?;
        then_env.add_assumption(cond_str.clone(), cond.clone());
        self.analyze_block(then_block, &mut then_env)?;

        let mut else_env = None;
        if let Some(else_block) = else_opt {
            let mut else_env_ = env.gen_new_env("else".to_string(), else_block.clone())?;
            else_env_.add_assumption(format!("(not {})", cond_str.clone()), cond);
            self.analyze_block(else_block, &mut else_env_)?;
            else_env = Some(else_env_)
        }

        env.merge_then_else_env(cond_str.clone(), then_env, else_env)?;
        Ok(())
    }

    fn analyze_block(
        &mut self, block: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, //expr
                                                      .. }, .. } = &*block {
            for stmt in stmts {
                self.analyze_expr(stmt.clone(), env)?;
            }
            // if let Some(expr) = expr {
            //     self.analyze_expr(expr.clone())?;
            // }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(())
    }

    // fn analyze_block(&mut self, block: Rc<RExpr<'tcx>>) -> Result<(), AnalysisError> {
    //     if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*block {
    //         for stmt in stmts {
    //             self.analyze_expr(stmt.clone())?;
    //         }
    //         if let Some(expr) = expr {
    //             self.analyze_expr(expr.clone())?;
    //         }
    //     } else {
    //         return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
    //     }
    //     Ok(())
    // }

    /// Constraint generation functions
    /// - expr_to_constraint
    /// - literal_to_constraint
    /// - var_ref_to_constraint
    /// - logical_op_to_constraint
    /// - un_op_to_constraint
    /// - bin_op_to_constraint
    /// - fn_to_constraint
    /// - extern_fn_to_constraint
    /// - value_to_ite
    /// - block_to_constraint

    fn expr_to_constraint(
        &mut self, arg: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        use RExprKind::*;

        match &arg.kind {
            Literal { lit, neg } => Ok(Analyzer::literal_to_constraint(lit, *neg)?),
            VarRef { id } => self.var_ref_to_constraint(id, env),
            LogicalOp { op, lhs, rhs } => {
                let lhs_str = self.expr_to_constraint(lhs.clone(), env)?;
                let rhs_str = self.expr_to_constraint(rhs.clone(), env)?;
                Ok(self.logical_op_to_constraint(*op, &lhs_str, &rhs_str)?)
            }
            Unary { op, arg } => {
                let arg_str = self.expr_to_constraint(arg.clone(), env)?;
                Ok(self.un_op_to_constraint(*op, &arg_str)?)
            }
            Binary { op, lhs, rhs } => {
                let lhs_str = self.expr_to_constraint(lhs.clone(), env)?;
                let rhs_str = self.expr_to_constraint(rhs.clone(), env)?;
                Ok(self.bin_op_to_constraint(*op, &lhs_str, &rhs_str)?)
            }
            Call { ty, args, .. } => self.fn_to_constraint(*ty, args.clone(), env),
            If { cond, then, else_opt } => {
                Ok(self.if_to_constraint(cond.clone(), then.clone(), else_opt.clone(), env)?)
            }
            _ => {
                println!("{}", env.get_assumptions()?);
                Err(AnalysisError::UnsupportedPattern(format!("name: {:?}", arg.kind)))
            }
        }
    }

    fn literal_to_constraint(lit: &'tcx Lit, neg: bool) -> Result<String, AnalysisError> {
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

    fn var_ref_to_constraint(
        &self, id: &LocalVarId, env: &Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        Ok(env.get_var(id).0)
    }

    fn logical_op_to_constraint(
        &mut self, op: LogicalOp, lhs_str: &String, rhs_str: &String,
    ) -> Result<String, AnalysisError> {
        use LogicalOp::*;

        let op_str = match op {
            And => "and",
            Or => "or",
        };
        Ok(format!("({} {} {})", op_str, lhs_str, rhs_str))
    }

    fn un_op_to_constraint(&mut self, op: UnOp, arg_str: &String) -> Result<String, AnalysisError> {
        use UnOp::*;

        let op_str = match op {
            Not => "not",
            Neg => "-",
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", op))),
        };
        Ok(format!("({} {})", op_str, arg_str))
    }

    fn bin_op_to_constraint(
        &mut self, op: BinOp, lhs_str: &String, rhs_str: &String,
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
        Ok(format!("({} {} {})", op_str, lhs_str, rhs_str))
    }

    fn fn_to_constraint(
        &mut self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        match ty.kind() {
            TyKind::FnDef(def_id, ..) => {
                let fn_info = self.get_fn_info(def_id);
                if let Some(fun) = self.get_local_fn(def_id) {
                    self.local_fn_to_constraint(fun.clone(), args, env)
                } else {
                    self.extern_fn_to_constraint(fn_info, args)
                }
            }
            _ => panic!("Call has not have FnDef"),
        }
    }

    fn local_fn_to_constraint(
        &mut self, expr: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        self.analyze_params(&expr.params, args, env)?;
        self.block_to_constraint(expr.body.as_ref().expect("Body not found").clone(), env)
    }

    fn extern_fn_to_constraint(
        &self, fn_info: Vec<String>, _: Box<[Rc<RExpr<'tcx>>]>,
    ) -> Result<String, AnalysisError> {
        if fn_info[0] == "t3modules" {
            match fn_info[1].as_str() {
                "rand_bool" => Err(AnalysisError::RandFunctions),
                "rand_int" => Err(AnalysisError::RandFunctions),
                "rand_float" => Err(AnalysisError::RandFunctions),
                _ => unreachable!(),
            }
        } else {
            Err(AnalysisError::UnsupportedPattern("Unknown function!".into()))
        }
    }

    fn if_to_constraint(
        &mut self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        let cond_str = self.expr_to_constraint(cond.clone(), env)?;

        let mut then_env = env.gen_new_env("then".to_string(), then_block.clone())?;
        then_env.add_assumption(cond_str.clone(), cond.clone());
        let then_value = self.block_to_constraint(then_block, &mut then_env)?;

        let else_block = else_opt.expect("Else block of if initializer not found");
        let mut else_env = env.gen_new_env("else".to_string(), else_block.clone())?;
        else_env.add_assumption(format!("(not {})", cond_str.clone()), cond);
        let else_value = self.block_to_constraint(else_block, &mut else_env)?;

        env.merge_then_else_env(cond_str.clone(), then_env, Some(else_env))?;
        Ok(Analyzer::value_to_ite(cond_str, then_value, else_value))
    }

    fn value_to_ite(cond_str: String, then_value: String, else_value: String) -> String {
        format!("(ite {} {} {})", cond_str, then_value, else_value)
    }

    fn block_to_constraint(
        &mut self, block: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        let mut return_value = String::new();
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*block {
            for stmt in stmts {
                if let AnalysisType::Return(value) = self.analyze_expr(stmt.clone(), env)? {
                    return Ok(value.expect("No value with return"));
                }
            }
            if let Some(expr) = expr {
                return_value = self.expr_to_constraint(expr.clone(), env)?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(return_value)
    }

    /// Special functions
    /// - analyze_t3assert
    /// - analyze_t3assume
    /// - analyze_invariant

    fn analyze_t3assert(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        self.analyze_t3assume(args, env)?;
        let smt = env.get_assumptions_for_verify()?;
        self.verify(smt, env)?;
        Ok(AnalysisType::Other)
    }

    fn analyze_t3assume(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        let constraint = self.expr_to_constraint(args[0].clone(), env)?;
        env.add_assumption(constraint, args[0].clone());
        Ok(AnalysisType::Other)
    }

    fn analyze_invariant(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        Ok(AnalysisType::Invariant(Vec::from_iter(args.iter()).remove(0).clone()))
    }
}
