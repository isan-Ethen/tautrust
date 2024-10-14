// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn analyze_main(&self, rthir: Rc<RThir<'tcx>>) -> Result<(), AnalysisError> {
        if let Some(body) = &rthir.body {
            let mut main_env = Env::new();
            self.analyze_body(body.clone(), &mut main_env)?;
        }
        Ok(())
    }

    pub fn analyze_params(
        &self, params: &Vec<RParam<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RExprKind::*;

        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pattern) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = pattern.as_ref() {
                    self.analyze_pattern(pattern, kind, arg, env)?
                }
            }
        }
        Ok(())
    }

    fn analyze_pattern(
        &self, pattern: &Rc<RExpr<'tcx>>, kind: &RPatKind<'tcx>, arg: &Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RPatKind::*;
        match kind {
            Binding { ty, var, .. } => {
                self.process_binding(pattern.clone(), Some(arg.clone()), ty, var, env)?
            }
            Wild => (),
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    pub fn analyze_body(
        &self, body: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = body.as_ref() {
            self.analyze_statements(stmts.iter().cloned(), env)?;
            if let Some(expr) = expr {
                self.analyze_expr(expr.clone(), env)?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(())
    }

    fn analyze_statements<I>(&self, stmts: I, env: &mut Env<'tcx>) -> Result<(), AnalysisError>
    where I: Iterator<Item = Rc<RExpr<'tcx>>> {
        for stmt in stmts {
            let return_value = self.analyze_expr(stmt, env)?;
            if matches!(return_value, AnalysisType::Return(..) | AnalysisType::Break) {
                break;
            }
        }
        Ok(())
    }

    pub fn analyze_expr(
        &self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        use RExprKind::*;

        match expr.kind.clone() {
            Literal { .. } => Ok(AnalysisType::Other),
            VarRef { .. } => Ok(AnalysisType::Other),
            Binary { lhs, rhs, .. } => handle_result(self.analyze_binary(rhs, lhs, env)),
            Pat { kind } => handle_result(self.analyze_pat(&kind, expr, env)),
            Call { ty, args, .. } => self.analyze_fn(ty, args, env),
            LetStmt { pattern, initializer } => {
                handle_result(self.analyze_let_stmt(pattern, initializer, env))
            }
            Return { value } => self.handle_return(value, env),
            AssignOp { op, lhs, rhs } => {
                handle_result(self.analyze_assign_op(op, lhs, rhs, expr, env))
            }
            Assign { lhs, rhs } => handle_result(self.analyze_assign(lhs, rhs, env)),
            If { cond, then, else_opt } => {
                handle_result(self.analyze_if(cond, then, else_opt, env))
            }
            Break { .. } => Ok(AnalysisType::Break),
            _ => Err(AnalysisError::UnsupportedPattern("Unknown expr".into())),
        }
    }

    fn handle_return(
        &self, value: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        if let Some(expr) = value {
            Ok(AnalysisType::Return(Some(self.expr_to_constraint(expr, env)?.get_assume().into())))
        } else {
            Ok(AnalysisType::Return(None))
        }
    }
}

fn handle_result<'tcx>(
    result: Result<(), AnalysisError>,
) -> Result<AnalysisType<'tcx>, AnalysisError> {
    match result {
        Ok(_) => Ok(AnalysisType::Other),
        Err(err) => Err(err),
    }
}
