// rustc crates
use rustc_middle::ty::*;

// std crates
// Own crates
use crate::analyze::{lir::*, *};

impl<'tcx> Analyzer<'tcx> {
    pub fn expr_to_constraint(
        &self, arg: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<LirKind<'tcx>, AnalysisError> {
        use RExprKind::*;

        match &arg.kind {
            Literal { lit, neg } => Ok(Analyzer::literal_to_constraint(lit, *neg)?),
            VarRef { id } => self.var_ref_to_constraint(id, env),
            // LogicalOp { op, lhs, rhs } => {
            //     let mut lhs = self.expr_to_constraint(lhs.clone(), env)?;
            //     let rhs = self.expr_to_constraint(rhs.clone(), env)?;
            //     let (op_str, rhs_str) = self.logical_op_to_constraint(*op, &rhs)?;
            //     lhs.adapt_assume(&op_str, rhs_str);
            //     Ok(lhs)
            // }
            // Unary { op, arg } => {
            //     let arg_str = self.expr_to_constraint(arg.clone(), env)?;
            //     Ok(self.un_op_to_constraint(*op, &arg_str)?)
            // }
            Binary { op, lhs, rhs } => {
                let mut lhs = self.expr_to_constraint(lhs.clone(), env)?;
                let rhs = self.expr_to_constraint(rhs.clone(), env)?;
                let (op_str, rhs_str) = self.bin_op_to_constraint(*op, &rhs)?;
                lhs.adapt_assume(&op_str, rhs_str);
                Ok(lhs)
            }
            // Call { ty, args, .. } => self.fn_to_constraint(*ty, args.clone(), env),
            // If { cond, then, else_opt } => {
            //     Ok(self.if_to_constraint(cond.clone(), then.clone(), else_opt.clone(), env)?)
            // }
            // Deref { arg } => Ok(self.expr_to_constraint(arg.clone(), env)?),
            // Borrow { arg } => Ok(self.expr_to_constraint(arg.clone(), env)?),
            _ => {
                println!("{}", env.get_assumptions()?);
                Err(AnalysisError::UnsupportedPattern(format!("name: {:?}", arg.kind)))
            }
        }
    }

    pub fn literal_to_constraint(
        lit: &'tcx Lit, neg: bool,
    ) -> Result<LirKind<'tcx>, AnalysisError> {
        match lit.node {
            LitKind::Int(n, _) => Ok(LirKind::new(
                TyKind::Int(IntTy::I32),
                if neg { format!("-{n}") } else { format!("{n}") },
            )),
            LitKind::Float(symbol, _) => Ok(LirKind::new(
                TyKind::Float(FloatTy::F64),
                if neg { format!("-{symbol}") } else { format!("{symbol}") },
            )),
            LitKind::Bool(b) => Ok(LirKind::new(TyKind::Bool, b.to_string())),
            _ => Err(AnalysisError::UnsupportedPattern(format!(
                "Unsupported literal pattern: {}",
                lit.node
            ))),
        }
    }

    pub fn var_ref_to_constraint(
        &self, id: &LocalVarId, env: &Env<'tcx>,
    ) -> Result<LirKind<'tcx>, AnalysisError> {
        Ok(env.var_map.get(id).expect("var not found in ver_ref_to_constraint").kind.clone())
    }

    // pub fn logical_op_to_constraint<'a>(
    //     &self, op: LogicalOp, rhs: &'a LirKind<'tcx>,
    // ) -> Result<(String, &'a String), AnalysisError> {
    //     let op_str = match op {
    //         LogicalOp::And => "and",
    //         LogicalOp::Or => "or",
    //     };
    //     Ok((op_str.to_string(), rhs.get_assume()))
    // }

    // pub fn un_op_to_constraint(
    //     &self, op: UnOp, arg_str: &String,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     use UnOp::*;

    //     let op_str = match op {
    //         Not => "not",
    //         Neg => "-",
    //         _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", op))),
    //     };
    //     Ok(format!("({} {})", op_str, arg_str))
    // }

    pub fn bin_op_to_constraint<'a>(
        &self, op: BinOp, rhs: &'a LirKind<'tcx>,
    ) -> Result<(String, &'a String), AnalysisError> {
        Ok((Analyzer::bin_op_to_smt(op)?, rhs.get_assume()))
    }

    pub fn bin_op_to_smt(op: BinOp) -> Result<String, AnalysisError> {
        use BinOp::*;

        let op_str = match op {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Rem => "mod",
            Div => "div",
            BitXor => "^",
            BitAnd => "&",
            BitOr => "|",
            Eq => "=",
            Lt => "<",
            Le => "<=",
            Ne => "distinct",
            Ge => ">=",
            Gt => ">",
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{op:?}"))),
        };
        Ok(op_str.to_string())
    }

    // pub fn fn_to_constraint(
    //     &self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     match ty.kind() {
    //         TyKind::FnDef(def_id, ..) => {
    //             let fn_info = self.get_fn_info(def_id);
    //             if let Some(fun) = self.get_local_fn(def_id) {
    //                 self.local_fn_to_constraint(fun.clone(), args, env)
    //             } else {
    //                 self.extern_fn_to_constraint(fn_info, args)
    //             }
    //         }
    //         _ => panic!("Call has not have FnDef"),
    //     }
    // }

    // pub fn local_fn_to_constraint(
    //     &self, expr: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     self.analyze_params(&expr.params, args, env)?;
    //     self.block_to_constraint(expr.body.as_ref().expect("Body not found").clone(), env)
    // }

    // pub fn extern_fn_to_constraint(
    //     &self, fn_info: Vec<String>, _: Box<[Rc<RExpr<'tcx>>]>,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     if fn_info[0] == "t3modules" {
    //         match fn_info[1].as_str() {
    //             "rand_bool" => Err(AnalysisError::RandFunctions),
    //             "rand_int" => Err(AnalysisError::RandFunctions),
    //             "rand_float" => Err(AnalysisError::RandFunctions),
    //             _ => unreachable!(),
    //         }
    //     } else {
    //         Err(AnalysisError::UnsupportedPattern("Unknown function!".into()))
    //     }
    // }

    // pub fn if_to_constraint(
    //     &self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
    //     else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     let cond = self.expr_to_constraint(cond, env)?;
    //     let cond_str = cond.get_assume();

    //     let mut then_env = env.gen_new_env()?;
    //     then_env.add_assume(cond_str.to_string());
    //     let mut then_value = self.block_to_constraint(then_block, &mut then_env)?;
    //     let else_block = else_opt.expect("Else block of if initializer not found");
    //     let mut else_env = env.gen_new_env()?;
    //     else_env.add_assume(format!("(not {cond_str})"));
    //     let else_value = self.block_to_constraint(else_block, &mut else_env)?;

    //     env.merge_then_else_env(cond_str.clone(), then_env, Some(else_env))?;
    //     then_value.set_assume(Analyzer::value_to_ite(
    //         cond_str,
    //         then_value.get_assume(),
    //         else_value.get_assume(),
    //     ));
    //     Ok(then_value)
    // }

    // pub fn value_to_ite(cond_str: &String, then_value: &String, else_value: &String) -> String {
    //     format!("(ite {cond_str} {then_value} {else_value})")
    // }

    // pub fn block_to_constraint(
    //     &self, block: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    // ) -> Result<LirKind<'tcx>, AnalysisError> {
    //     let mut return_value = LirKind::new(TyKind::Int(IntTy::I32), String::new());
    //     if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = block.as_ref() {
    //         for stmt in stmts {
    //             if let AnalysisType::Return(value) = self.analyze_expr(stmt.clone(), env)? {
    //                 return Err(AnalysisError::UnsupportedPattern(
    //                     value.expect("No value with return"),
    //                 ));
    //             }
    //         }
    //         if let Some(expr) = expr {
    //             return_value = self.expr_to_constraint(expr.clone(), env)?;
    //         }
    //     } else {
    //         println!("{}", env.get_assumptions()?);
    //         println!("{:?}", block);
    //         return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
    //     }
    //     Ok(return_value)
    // }
}
