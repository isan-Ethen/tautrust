// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_span::{def_id::LocalDefId, Span};

// std crates
use std::collections::{HashMap as Map, VecDeque};
use std::io::Write;
use std::process::Command;
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;
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
    var_map: Map<LocalVarId, Rc<RExpr<'tcx>>>,
}

#[derive(Debug)]
pub enum AnalysisError {
    FunctionNotFound(LocalDefId),
    UnsupportedPattern(String),
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
        analyzer.analyze_fn(main)
    }

    fn verify(&self) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .arg("-in")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut stdin = child.stdin.take().expect("Open std failed");
        stdin.write_all(self.get_current_assumptions().as_bytes()).expect("Write smt failed");
        drop(stdin);

        let output = child.wait_with_output().expect("Get stdout failed");
        let result = String::from_utf8(output.stdout).expect("Load result failed");
        if &result != "unsat" {
            return Err(AnalysisError::VerifyError {
                message: result,
                span: self.get_current_span(),
            });
        }

        Ok(())
    }

    fn get_current_assumptions(&self) -> String {
        let mut smt = String::new();
        for lir in self.current_path.iter() {
            smt.push_str(&lir.to_smt());
        }
        smt
    }

    fn get_current_span(&self) -> Span {
        self.current_path.back().expect("Lir not found").get_span()
    }

    fn get_fn(&self, fn_id: LocalDefId) -> Result<Rc<RThir<'tcx>>, AnalysisError> {
        self.fn_map.get(&fn_id).cloned().ok_or(AnalysisError::FunctionNotFound(fn_id))
    }

    fn analyze_fn(&mut self, rthir: Rc<RThir<'tcx>>) -> Result<(), AnalysisError> {
        self.analyze_params(&rthir.params)?;
        if let Some(body) = &rthir.body {
            self.analyze_body((*body).clone())?;
        }
        Ok(())
    }

    fn analyze_params(&mut self, params: &[RParam<'tcx>]) -> Result<(), AnalysisError> {
        use RExprKind::*;
        use RPatKind::*;

        for param in params {
            if let Some(pat) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = &**pat {
                    match kind {
                        Binding { name, ty, var, .. } => {
                            let parameter =
                                Lir::<'tcx>::new_parameter(name.clone(), ty.clone(), pat.clone());
                            self.current_path.push_back(parameter);
                            self.var_map.insert(var.clone(), pat.clone());
                        }
                        Wild => (),
                        _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
                    }
                }
            }
        }
        Ok(())
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
                self.var_map.insert(var.clone(), pat.clone());
            }
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    fn analyze_body(&mut self, body: Rc<RExpr<'tcx>>) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*body {
            for stmt in stmts {
                self.analyze_expr(stmt.clone())?;
            }
            if let Some(expr) = expr {
                self.analyze_expr(expr.clone())?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: Rc<RExpr<'tcx>>) -> Result<(), AnalysisError> {
        use RExprKind::*;

        let kind = &expr.kind;
        let expr = expr.clone();

        match kind {
            Pat { kind } => self.analyze_pat(&kind, expr)?,
            // If { cond, then, else_opt } => {
            //     self.format_expr(cond);
            //     self.format_expr(then);
            //     if let Some(else_expr) = else_opt {
            //         self.format_expr(else_expr);
            //     }
            // }
            // Call { fun, args, ty, from_hir_call, fn_span } => {
            //     self.format_expr(fun);
            //     if args.len() > 0 {
            //         for arg in args.iter() {
            //             self.format_expr(arg);
            //         }
            //     } else {
            //     }
            // }
            // Deref { arg } => {
            //     self.format_expr(arg);
            // }
            // Binary { op, lhs, rhs } => {
            //     self.format_expr(lhs);
            //     self.format_expr(rhs);
            // }
            // LogicalOp { op, lhs, rhs } => {
            //     self.format_expr(lhs);
            //     self.format_expr(rhs);
            // }
            // Unary { op, arg } => {
            //     self.format_expr(arg);
            // }
            // Cast { source } => {
            //     self.format_expr(source);
            // }
            // PointerCoercion { cast, source } => {
            //     self.format_expr(source);
            // }
            // Loop { body } => {
            //     self.format_expr(body);
            // }
            // LetBinding { expr, pat } => {
            //     self.format_expr(expr);
            // }
            // Match { scrutinee, arms, .. } => {
            //     self.format_expr(scrutinee);
            //     for arm_id in arms.iter() {
            //         self.format_expr(arm_id);
            //     }
            // }
            // Block { stmts, expr } => {
            //     if stmts.len() > 0 {
            //         for stmt in stmts.iter() {
            //             self.format_expr(stmt);
            //         }
            //     } else {
            //     }
            //     if let Some(expr) = expr {
            //         self.format_expr(expr);
            //     } else {
            //     }
            // }
            // Assign { lhs, rhs } => {
            //     self.format_expr(lhs);
            //     self.format_expr(rhs);
            // }
            // AssignOp { op, lhs, rhs } => {
            //     self.format_expr(lhs);
            //     self.format_expr(rhs);
            // }
            // Field { lhs, variant_index, name } => {
            //     self.format_expr(lhs);
            // }
            // Index { lhs, index } => {
            //     self.format_expr(lhs);
            // }
            // VarRef { id } => {
            // }
            // UpvarRef { closure_def_id, var_hir_id } => {
            //         &format!("closure_def_id: {:?}", closure_def_id),
            //         depth_lvl + 1,
            //     );
            // }
            // Borrow { borrow_kind, arg } => {
            //     self.format_expr(arg);
            // }
            // Break { label, value } => {
            //     if let Some(value) = value {
            //         self.format_expr(value);
            //     }
            // }
            // Continue { label } => {
            // }
            // Return { value } => {
            //     if let Some(value) = value {
            //         self.format_expr(value);
            //     }
            // }
            // Repeat { value, count } => {
            //     self.format_expr(value);
            // }
            // Array { fields } => {
            //     for field in fields.iter() {
            //         self.format_expr(field);
            //     }
            // }
            // Tuple { fields } => {
            //     for field_id in fields.iter() {
            //         self.format_expr(field_id);
            //     }
            // }
            // PlaceTypeAscription { source, user_ty } => {
            //     self.format_expr(source);
            // }
            // ValueTypeAscription { source, user_ty } => {
            //     self.format_expr(source);
            // }
            // Literal { lit, neg } => {
            // }
            // NonHirLiteral { lit, user_ty } => {
            // }
            // ZstLiteral { user_ty } => {
            // }
            // NamedConst { def_id, args, user_ty } => {
            // }
            // ConstParam { param, def_id } => {
            // }
            LetStmt { pattern, initializer: _, else_block: _ } => {
                self.analyze_expr(pattern.clone())?;

                // if let Some(init) = initializer {
                //     self.analyze_expr(init.clone())?;
                // }

                // if let Some(else_block) = else_block {
                //     self.analyze_expr(else_block.clone())?;
                // }
            }
            // Arm { pattern, guard, body } => {
            //     self.format_expr(pattern);
            //     if let Some(guard) = guard {
            //         self.format_expr(guard);
            //     } else {
            //     }
            //     self.format_expr(body);
            // }
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", expr.kind))),
        }
        Ok(())
    }
}
