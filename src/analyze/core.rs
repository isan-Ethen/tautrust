// rustc crates
use rustc_middle::ty::Mutability;

// std crates
// Own crates
use crate::analyze::{lir::*, *};

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
        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pattern) = &param.pat {
                self.analyze_pattern(arg, pattern, env)?;
            }
        }
        Ok(())
    }

    fn analyze_pattern(
        &self, arg: &Rc<RExpr<'tcx>>, pattern: &Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Pat { kind }, .. } = pattern.as_ref() {
            match kind {
                RPatKind::Binding { ty, var, .. } => {
                    self.handle_binding(ty, var, arg, pattern, env)?
                }
                RPatKind::Wild => (),
                _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
            }
        }
        Ok(())
    }

    fn handle_binding(
        &self, ty: &Ty<'tcx>, var: &LocalVarId, arg: &Rc<RExpr<'tcx>>, pattern: &Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        match ty.kind() {
            TyKind::Ref(_, ref_ty, mutability) => {
                self.handle_ref_binding(mutability, ref_ty, var, arg, pattern, env)?
            }
            _ => self.add_parameter_and_assign(ty, var, arg, pattern, env)?,
        }
        Ok(())
    }

    fn handle_ref_binding(
        &self, mutability: &Mutability, ref_ty: &Ty<'tcx>, var: &LocalVarId, arg: &Rc<RExpr<'tcx>>,
        pattern: &Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        match mutability {
            Mutability::Not => self.add_parameter_and_assign(ref_ty, var, arg, pattern, env),
            Mutability::Mut => self.handle_mutable_binding(var, arg, pattern, ref_ty, env),
        }
    }

    fn handle_mutable_binding(
        &self, var: &LocalVarId, arg: &Rc<RExpr<'tcx>>, pattern: &Rc<RExpr<'tcx>>, ty: &Ty<'tcx>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let name = Analyzer::span_to_str(&pattern.span);
        match &arg.kind {
            RExprKind::Borrow { arg } => self.handle_borrow(arg, var, name, ty, pattern, env),
            RExprKind::If { cond, then, else_opt } => self.handle_if(
                cond.clone(),
                then.clone(),
                else_opt.clone(),
                var,
                name,
                ty,
                pattern,
                env,
            ),
            RExprKind::Deref { arg } => self.add_parameter_and_assign(ty, var, arg, pattern, env),
            _ => panic!("Unsupported mutable reference initializer: {:?}", pattern.span),
        }
    }

    fn add_parameter_and_assign(
        &self, ty: &Ty<'tcx>, var: &LocalVarId, arg: &Rc<RExpr<'tcx>>, pattern: &Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        env.add_parameter(ty.kind(), var, pattern.clone());
        let constraint = self.expr_to_constraint(arg.clone(), env)?;
        env.assign_new_value(var, constraint.get_assume().to_string());
        Ok(())
    }

    fn handle_borrow(
        &self, arg: &Rc<RExpr<'tcx>>, var: &LocalVarId, name: String, ty: &Ty<'tcx>,
        pattern: &Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExprKind::VarRef { id } = arg.kind {
            let mut_init = env.var_map.get_mut(&id).expect("var not found in Mutable");
            let temp = mut_init.get_assume().clone();
            mut_init.set_assume(name.clone());
            env.smt_vars.push((name.clone(), ty.kind().clone()));
            let lir = Lir::new(ty.kind().clone(), vec![temp, name], pattern.clone()).unwrap();
            env.add_mutable_ref(var, lir);
        }
        Ok(())
    }

    fn handle_if(
        &self, cond: Rc<RExpr<'tcx>>, then: Rc<RExpr<'tcx>>, else_opt: Option<Rc<RExpr<'tcx>>>,
        var: &LocalVarId, name: String, ty: &Ty<'tcx>, pattern: &Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let lir = self.if_to_mut(cond, then, else_opt, env)?;
        env.smt_vars.push((name.clone(), ty.kind().clone()));
        let lir = Lir { kind: lir, expr: pattern.clone() };
        env.add_mutable_ref(var, lir);
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
            let return_value = self.analyze_expr(stmt.clone(), env)?;
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
            Ok(AnalysisType::Return(Some(
                self.expr_to_constraint(expr, env)?.get_assume().to_string(),
            )))
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
