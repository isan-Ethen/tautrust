// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn analyze_main(&self, rthir: Rc<RThir<'tcx>>) -> Result<(), AnalysisError> {
        if let Some(body) = &rthir.body {
            let mut main_env = Env::new();
            self.analyze_body((*body).clone(), &mut main_env)?;
        }
        Ok(())
    }

    pub fn analyze_params(
        &self, params: &Vec<RParam<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RExprKind::*;
        use RPatKind::*;

        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pat) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = pat.as_ref() {
                    match kind {
                        Binding { ty, var, .. } => {
                            env.add_parameter(ty, var, pat.clone());
                            let constraint = self.expr_to_constraint(arg.clone(), env)?;
                            env.assign_new_value(var, constraint);
                        }
                        Wild => (),
                        _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
                    }
                }
            }
        }
        Ok(())
    }

    pub fn analyze_body(
        &self, body: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = body.as_ref() {
            let mut stmts_iter = stmts.iter().cloned().peekable();
            while let Some(stmt) = stmts_iter.next() {
                let return_value = self.analyze_expr(stmt.clone(), env)?;
                match &return_value {
                    AnalysisType::Return(..) => break,
                    AnalysisType::Invariant(_expr) => {
                        // self.analyze_loop(expr.clone(), &mut stmts_iter, env)?
                    }
                    AnalysisType::Break => break,
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

    pub fn analyze_expr(
        &self, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        use RExprKind::*;

        let mut return_value = AnalysisType::Other;

        match expr.kind.clone() {
            Literal { .. } => (), // self.analyze_literal(expr, env)?,
            VarRef { .. } => (),  // self.analyze_var_ref(expr, env)?,
            Binary { lhs, rhs, .. } => self.analyze_binary(rhs, lhs, env)?,
            Pat { kind } => self.analyze_pat(&kind, expr, env)?,
            Call { ty, args, .. } => return_value = self.analyze_fn(ty, args, env)?,
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
            Assign { lhs, rhs } => self.analyze_assign(lhs, rhs, env)?,
            If { cond, then, else_opt } => self.analyze_if(cond, then, else_opt, env)?,
            Break { .. } => return_value = AnalysisType::Break,
            _ => {
                println!("{:?}", expr.kind);
                return Err(AnalysisError::UnsupportedPattern("Unknown expr".into()));
            }
        }
        Ok(return_value)
    }
}
