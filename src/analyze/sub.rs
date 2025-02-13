// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_middle::ty::Mutability;

// std crates
// Own crates
use crate::analyze::{lir::*, *};

impl<'tcx> Analyzer<'tcx> {
    pub fn analyze_local_fn(
        &self, rthir: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.analyze_params(&rthir.params, args, env)?;
        if let Some(body) = &rthir.body {
            self.analyze_body(body.clone(), env)?;
        }
        Ok(())
    }

    pub fn analyze_binary(
        &self, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.analyze_expr(lhs, env)?;
        self.analyze_expr(rhs, env)?;
        Ok(())
    }

    pub fn analyze_pat(
        &self, kind: &RPatKind<'tcx>, pat: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RPatKind::*;

        match kind {
            // Wild => (),
            Binding { ty, var, .. } => env.add_parameter(ty.kind(), var, pat),
            // Deref { subpattern } => match self.analyze_expr(subpattern.clone(), env) {
            //     Ok(_) => (),
            //     Err(err) => return Err(err),
            // },
            // _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    pub fn analyze_fn(
        &self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        match ty.kind() {
            TyKind::FnDef(def_id, ..) => match self.get_local_fn(def_id) {
                Some(fun) => match self.analyze_local_fn(fun, args, env) {
                    Ok(()) => Ok(AnalysisType::Other),
                    Err(why) => Err(why),
                },
                _ => {
                    let fn_info = self.get_fn_info(def_id);
                    self.analyze_external_fn(fn_info, args, env)
                }
            },
            _ => panic!("Call has not have FnDef"),
        }
    }

    pub fn analyze_external_fn(
        &self, fn_info: Vec<String>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        if fn_info[0] == "t3modules" {
            match fn_info[1].as_str() {
                "t3assert" => self.analyze_t3assert(args, env),
                "t3assume" => self.analyze_t3assume(args, env),
                "t3drop" => self.analyze_drop(args, env),
                // "invariant" => self.analyze_invariant(args),
                _ => unreachable!(),
            }
        } else {
            Err(AnalysisError::UnsupportedPattern("Unknown function!".into()))
        }
    }

    pub fn analyze_let_stmt(
        &self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        match &pattern.clone().kind {
            RExprKind::Pat { kind: RPatKind::Binding { ty, var, .. } } => {
                self.process_binding(pattern, initializer, ty, var, env)?
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn process_binding(
        &self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>, ty: &Ty<'tcx>,
        var: &LocalVarId, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        match ty.kind() {
            TyKind::Ref(_region, ref_ty, mutability) => match mutability {
                Mutability::Not => self.process_binding_sub(pattern, initializer, ref_ty, var, env),
                Mutability::Mut => self.process_mutable_ref(pattern, initializer, ty, var, env),
            },
            _ => self.process_binding_sub(pattern, initializer, ty, var, env),
        }
    }

    pub fn process_mutable_ref(
        &self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>, ty: &Ty<'tcx>,
        var: &LocalVarId, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let Some(init) = initializer {
            match &init.kind {
                RExprKind::Borrow { .. } | RExprKind::VarRef { .. } => {
                    let prophecy_var = Analyzer::span_to_str(&pattern.span);
                    let var_ref =
                        if let RExprKind::Borrow { arg } = &init.kind { arg } else { &init };
                    Analyzer::process_var_ref_mut(pattern, var_ref, prophecy_var, ty, var, env);
                }
                RExprKind::If { cond, then, else_opt } => {
                    let lir = self.if_to_mut(cond.clone(), then.clone(), else_opt.clone(), env)?;
                    let lir = Lir { kind: lir, expr: pattern };
                    env.add_mutable_ref(var, lir);
                }
                RExprKind::Call { ty, args, .. } => {
                    let return_value = self.fn_to_constraint(*ty, args.clone(), env)?;
                    let lir = Lir { kind: return_value, expr: pattern };
                    env.add_mutable_ref(var, lir);
                }
                RExprKind::Deref { arg } => {
                    self.process_mutable_ref(pattern, Some(arg.clone()), ty, var, env)?
                }
                _ => {
                    println!("{init:?}");
                    unimplemented!(
                        "Other mutable reference initializer is not supported: {:?}",
                        pattern.span
                    )
                }
            }
        }
        Ok(())
    }

    pub fn process_var_ref_mut(
        pattern: Rc<RExpr<'tcx>>, arg: &RExpr<'tcx>, prophecy_var: String, ty: &Ty<'tcx>,
        var: &LocalVarId, env: &mut Env<'tcx>,
    ) {
        match arg.kind {
            RExprKind::VarRef { id } => {
                let mut_init = env.var_map.get_mut(&id).expect("var not found in Mutable");
                let var_expr = mut_init.get_var_expr().clone();
                env.smt_vars.push((prophecy_var.clone(), *ty.kind()));
                mut_init.set_var_expr(prophecy_var.clone());
                let lir = lir::Lir::new(*ty.kind(), vec![var_expr, prophecy_var], pattern).unwrap();
                env.add_mutable_ref(&var, lir);
            }
            _ => unreachable!("{arg:?} is in Borrow instead of VarRef"),
        }
    }

    pub fn process_binding_sub(
        &self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>, ty: &Ty<'tcx>,
        var: &LocalVarId, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        env.add_parameter(ty.kind(), &var, pattern.clone());
        if let Some(expr) = initializer {
            match self.expr_to_constraint(expr, env) {
                Ok(value) => env.assign_new_value(var, value.get_var_expr().into()),
                Err(AnalysisError::RandFunctions) => {
                    let rand = format!("rand_{}", Analyzer::span_to_str(&pattern.span));
                    env.add_rand(rand.clone(), ty.kind());
                    env.assign_new_value(var, rand)
                }
                Err(err) => return Err(err),
            }
        }
        Ok(())
    }

    pub fn if_to_mut(
        &self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<LirKind<'tcx>, AnalysisError> {
        let cond = self.expr_to_constraint(cond, env)?;
        let cond_str = cond.get_var_expr();

        let mut then_env = env.clone();
        then_env.add_assume(cond_str.clone());
        let mut then_value = self.block_to_constraint(then_block, &mut then_env)?;

        let else_block = else_opt.expect("Else block of if initializer not found");
        let mut else_env = env.clone();
        else_env.add_assume(format!("(not {cond_str})"));
        let else_value = self.block_to_constraint(else_block, &mut else_env)?;

        env.merge_then_else_env(cond_str.clone(), then_env, Some(else_env))?;
        then_value.set_var_expr(Analyzer::value_to_ite(
            cond_str,
            then_value.get_var_expr(),
            else_value.get_var_expr(),
        ));
        then_value.set_var_expr_by_index(
            Analyzer::value_to_ite(
                cond_str,
                then_value.get_var_expr_by_index(vec![1]),
                else_value.get_var_expr_by_index(vec![1]),
            ),
            vec![1],
        );
        Ok(then_value)
    }

    pub fn analyze_assign(
        &self, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let constraint = self.expr_to_constraint(rhs, env)?;
        let var = env.var_map.get_mut(&Analyzer::expr_to_id(lhs)).expect("Assign target not found");
        var.set_var_expr(constraint.get_var_expr().into());
        Ok(())
    }

    pub fn analyze_assign_op(
        &self, op: BinOp, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let rhs = self.expr_to_constraint(rhs, env)?;
        let op_str = Analyzer::bin_op_to_smt(op)?;
        env.add_assumption(
            &Analyzer::expr_to_id(lhs),
            op_str,
            rhs.get_var_expr().to_string(),
            expr,
        );
        Ok(())
    }

    pub fn analyze_if(
        &self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let cond_str: String = self.expr_to_constraint(cond.clone(), env)?.get_var_expr().into();

        let mut then_env = env.clone();
        then_env.add_assume(cond_str.clone());
        self.analyze_body(then_block, &mut then_env)?;

        let mut else_env = None;
        if let Some(else_block) = else_opt {
            let mut else_env_ = env.clone();
            else_env_.add_assume(format!("(not {})", cond_str));
            self.analyze_body(else_block, &mut else_env_)?;
            else_env = Some(else_env_)
        }

        env.merge_then_else_env(cond_str.clone(), then_env, else_env)?;
        Ok(())
    }
}
