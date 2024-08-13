// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn analyze_local_fn(
        &self, rthir: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.analyze_params(&rthir.params, args, env)?;
        if let Some(body) = &rthir.body {
            self.analyze_body((*body).clone(), env)?;
        }
        Ok(())
    }
    pub fn analyze_binary(
        &self, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.analyze_expr(lhs.clone(), env)?;
        self.analyze_expr(rhs.clone(), env)?;
        Ok(())
    }

    pub fn analyze_pat(
        &self, kind: &RPatKind<'tcx>, pat: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        use RPatKind::*;

        match kind {
            Wild => (),
            Binding { name, ty, var, .. } => env.add_parameter(name.to_string(), ty, var, pat),
            Deref { subpattern } => match self.analyze_expr(subpattern.clone(), env) {
                Ok(_) => (),
                Err(err) => return Err(err),
            },
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
        }
        Ok(())
    }

    pub fn analyze_fn(
        &self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        match ty.kind() {
            TyKind::FnDef(def_id, ..) => {
                let fn_info = self.get_fn_info(def_id);
                if let Some(fun) = self.get_local_fn(def_id) {
                    match self.analyze_local_fn(fun, args, env) {
                        Ok(()) => {
                            // env.merge_env(fn_env);
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

    pub fn analyze_extern_fn(
        &self, fn_info: Vec<String>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        if fn_info[0] == "t3modules" {
            match fn_info[1].as_str() {
                "t3assert" => self.analyze_t3assert(args, env),
                "t3assume" => self.analyze_t3assume(args, env),
                "invariant" => self.analyze_invariant(args),
                "t3drop" => self.analyze_drop(args),
                _ => unreachable!(),
            }
        } else {
            Err(AnalysisError::UnsupportedPattern("Unknown function!".into()))
        }
    }

    pub fn analyze_let_stmt(
        &self, pattern: Rc<RExpr<'tcx>>, initializer: Option<Rc<RExpr<'tcx>>>,
        _: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExprKind::Pat { kind: RPatKind::Binding { ty, var, .. } } = &pattern.kind {
            let name = Analyzer::span_to_str(&pattern.span);
            env.add_parameter(name.clone(), ty, &var.clone(), pattern.clone());
            if let Some(init) = initializer {
                match self.expr_to_constraint(init.clone(), env) {
                    Ok(value) => env.assign_new_value(var, value),
                    Err(err) => match err {
                        AnalysisError::RandFunctions => {
                            let rand = format!("rand_{}", Analyzer::span_to_str(&pattern.span));
                            env.add_rand(rand.clone(), ty);
                            env.assign_new_value(var, rand)
                        }
                        _ => return Err(err),
                    },
                }
            }
        } else {
            unreachable!();
        }
        Ok(())
    }

    pub fn analyze_assign_op(
        &self, op: BinOp, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let rhs = self.expr_to_constraint(rhs, env)?;
        let op_str = Analyzer::bin_op_to_smt(op)?;
        env.add_assumption(&Analyzer::expr_to_id(lhs), op_str, rhs, expr);
        Ok(())
    }

    pub fn analyze_assign(
        &self, lhs: Rc<RExpr<'tcx>>, rhs: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let constraint = self.expr_to_constraint(rhs, env)?;
        let var = env.var_map.get_mut(&Analyzer::expr_to_id(lhs)).expect("Assign target not found");
        var.assume = constraint;
        Ok(())
    }

    pub fn analyze_if(
        &self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let cond_str = self.expr_to_constraint(cond.clone(), env)?;

        let mut then_env = env.gen_new_env("then".to_string())?;
        then_env.add_assume(cond_str.clone());
        self.analyze_block(then_block, &mut then_env)?;

        let mut else_env = None;
        if let Some(else_block) = else_opt {
            let mut else_env_ = env.gen_new_env("else".to_string())?;
            else_env_.add_assume(format!("(not {})", cond_str.clone()));
            self.analyze_block(else_block, &mut else_env_)?;
            else_env = Some(else_env_)
        }

        env.merge_then_else_env(cond_str.clone(), then_env, else_env)?;
        Ok(())
    }

    pub fn analyze_block(
        &self, block: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        if let RExpr { kind: RExprKind::Block { stmts, //expr
                                                      .. }, .. } = block.as_ref() {
            for stmt in stmts {
                self.analyze_expr(stmt.clone(), env)?;
            }
        } else {
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(())
    }
}