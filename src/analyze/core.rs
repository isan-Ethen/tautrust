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
        use RExprKind::*;
        use RPatKind::*;

        for (param, arg) in params.iter().zip(args.iter()) {
            if let Some(pattern) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = pattern.as_ref() {
                    match kind {
                        Binding { ty, var, .. } =>
                        // env.add_parameter(ty, var, pat.clone());
                        // let constraint = self.expr_to_constraint(arg.clone(), env)?;
                        // env.assign_assume(var, constraint.clone());
                        {
                            match ty.kind() {
                                TyKind::Ref(_region, ref_ty, mutability) => match mutability {
                                    Mutability::Not => {
                                        env.add_parameter(
                                            ref_ty.kind(),
                                            &var.clone(),
                                            pattern.clone(),
                                        );
                                        match self.expr_to_constraint(arg.clone(), env) {
                                            Ok(value) => env.assign_new_value(
                                                var,
                                                value.get_assume().to_string(),
                                            ),
                                            Err(err) => match err {
                                                AnalysisError::RandFunctions => {
                                                    let rand = format!(
                                                        "rand_{}",
                                                        Analyzer::span_to_str(&pattern.span)
                                                    );
                                                    env.add_rand(rand.clone(), ref_ty.kind());
                                                    env.assign_new_value(var, rand)
                                                }
                                                _ => return Err(err),
                                            },
                                        }
                                    }
                                    Mutability::Mut => {
                                        let name = Analyzer::span_to_str(&pattern.span);
                                        match &arg.kind {
                                            RExprKind::Borrow { arg } => {
                                                if let RExprKind::VarRef { id } = arg.kind {
                                                    let mut_init = env
                                                        .var_map
                                                        .get_mut(&id)
                                                        .expect("var not found in Mutable");
                                                    let temp = mut_init.get_assume().clone();
                                                    mut_init.set_assume(name.clone());
                                                    env.smt_vars
                                                        .push((name.clone(), ty.kind().clone()));
                                                    let lir = lir::Lir::new(
                                                        ty.kind().clone(),
                                                        vec![temp, name],
                                                        pattern.clone(),
                                                    )
                                                    .unwrap();
                                                    env.add_mutable_ref(&var.clone(), lir);
                                                }
                                            }
                                            RExprKind::If { cond, then, else_opt } => {
                                                let lir = self.if_to_mut(
                                                    cond.clone(),
                                                    then.clone(),
                                                    else_opt.clone(),
                                                    env,
                                                )?;
                                                env.smt_vars
                                                    .push((name.clone(), ty.kind().clone()));
                                                let lir = Lir { kind: lir, expr: pattern.clone() };
                                                env.add_mutable_ref(&var.clone(), lir);
                                            }
                                            RExprKind::Deref { arg } => {
                                                env.add_parameter(ty.kind(), var, arg.clone());
                                                let constraint =
                                                    self.expr_to_constraint(arg.clone(), env)?;
                                                env.assign_assume(var, constraint.clone());
                                            }
                                            _ => {
                                                println!("{:?}", arg);
                                                panic!("Other mutable reference initializer is not supported: {:?}", pattern.span)
                                            }
                                        }
                                    }
                                },
                                _ => {
                                    env.add_parameter(ty.kind(), &var.clone(), pattern.clone());
                                    match self.expr_to_constraint(arg.clone(), env) {
                                        Ok(value) => env
                                            .assign_new_value(var, value.get_assume().to_string()),
                                        Err(err) => match err {
                                            AnalysisError::RandFunctions => {
                                                let rand = format!(
                                                    "rand_{}",
                                                    Analyzer::span_to_str(&pattern.span)
                                                );
                                                env.add_rand(rand.clone(), ty.kind());
                                                env.assign_new_value(var, rand)
                                            }
                                            _ => return Err(err),
                                        },
                                    }
                                }
                            }
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
                    return_value = AnalysisType::Return(Some(
                        self.expr_to_constraint(expr, env)?.get_assume().to_string(),
                    ));
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
