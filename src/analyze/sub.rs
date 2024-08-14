// rustc crates
use rustc_middle::ty::{FloatTy, IntTy, Mutability};

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
            Binding { ty, var, .. } => env.add_parameter(ty, var, pat),
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
                        Ok(()) => Ok(AnalysisType::Other),
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
                "t3drop" => self.analyze_drop(args, env),
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
            match ty.kind() {
                TyKind::Ref(_region, ref_ty, mutability) => match mutability {
                    Mutability::Not => {
                        env.add_parameter(ref_ty, &var.clone(), pattern.clone());
                        if let Some(init) = initializer {
                            match self.expr_to_constraint(init.clone(), env) {
                                Ok(value) => env.assign_new_value(var, value),
                                Err(err) => match err {
                                    AnalysisError::RandFunctions => {
                                        let rand = format!(
                                            "rand_{}",
                                            Analyzer::span_to_str(&pattern.span)
                                        );
                                        env.add_rand(rand.clone(), ref_ty);
                                        env.assign_new_value(var, rand)
                                    }
                                    _ => return Err(err),
                                },
                            }
                        }
                    }
                    Mutability::Mut => {
                        if let Some(init) = initializer {
                            let name = Analyzer::span_to_str(&pattern.span);
                            match &init.kind {
                                RExprKind::Borrow { arg } => {
                                    if let RExprKind::VarRef { id } = arg.kind {
                                        let mut_init = env
                                            .var_map
                                            .get_mut(&id)
                                            .expect("var not found in Mutable");
                                        let temp = mut_init.get_assume().clone();
                                        mut_init.set_assume(name.clone());
                                        env.smt_vars.push((name.clone(), ty.clone()));
                                        let lir = lir::Lir::new(
                                            ty.clone(),
                                            vec![temp, name],
                                            pattern.clone(),
                                        )
                                        .unwrap();
                                        env.add_mutable_ref(&var.clone(), lir);
                                    }
                                }
                                // RExprKind::If { cond, then, else_opt } => {}
                                _ => {
                                    println!("{:?}", init);
                                    panic!("Other mutable reference initializer is not supported: {:?}", pattern.span)
                                }
                            }
                        }
                    }
                },
                _ => {
                    env.add_parameter(ty, &var.clone(), pattern.clone());
                    fn assume_ty_range(name: String, ty: Ty<'_>, env: &mut Env<'_>) {
                        match ty.kind() {
                            TyKind::Int(int_ty) => {
                                fn int_range<T: From<i128>>(int_ty: IntTy) -> (T, T) {
                                    match int_ty {
                                        IntTy::Isize => (
                                            T::from(isize::MIN as i128),
                                            T::from(isize::MAX as i128),
                                        ),
                                        IntTy::I8 => {
                                            (T::from(i8::MIN as i128), T::from(i8::MAX as i128))
                                        }
                                        IntTy::I16 => {
                                            (T::from(i16::MIN as i128), T::from(i16::MAX as i128))
                                        }
                                        IntTy::I32 => {
                                            (T::from(i32::MIN as i128), T::from(i32::MAX as i128))
                                        }
                                        IntTy::I64 => {
                                            (T::from(i64::MIN as i128), T::from(i64::MAX as i128))
                                        }
                                        IntTy::I128 => (T::from(i128::MIN), T::from(i128::MAX)),
                                    }
                                }
                                let (min, max): (i128, i128) = int_range(int_ty.clone());
                                env.add_assume(format!(
                                    "(and (>= {} {}) (<= {} {}))",
                                    name, min, name, max
                                ))
                            }
                            TyKind::Float(float_ty) => {
                                fn float_range<T: From<f64>>(float_ty: FloatTy) -> (T, T) {
                                    match float_ty {
                                        FloatTy::F32 => {
                                            (T::from(f32::MIN as f64), T::from(f32::MAX as f64))
                                        }
                                        FloatTy::F64 => (T::from(f64::MIN), T::from(f64::MAX)),
                                        _ => panic!("type {:?} is not supported!", float_ty),
                                    }
                                }
                                let (min, max): (f64, f64) = float_range(float_ty.clone());
                                env.add_assume(format!(
                                    "(and (>= {} {}) (<= {} {}))",
                                    name, min, name, max
                                ))
                            }
                            _ => (),
                        }
                    }
                    if let Some(init) = initializer {
                        match self.expr_to_constraint(init.clone(), env) {
                            Ok(value) => {
                                env.assign_new_value(var, value.clone());
                            }
                            Err(err) => match err {
                                AnalysisError::RandFunctions => {
                                    let rand =
                                        format!("rand_{}", Analyzer::span_to_str(&pattern.span));
                                    env.add_rand(rand.clone(), ty);
                                    env.assign_new_value(var, rand.clone());
                                    assume_ty_range(rand.clone(), ty.clone(), env)
                                }
                                _ => return Err(err),
                            },
                        }
                    }
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
        var.set_assume(constraint);
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
