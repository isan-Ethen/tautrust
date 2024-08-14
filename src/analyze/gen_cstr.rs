// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn expr_to_constraint(
        &self, arg: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
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
            Deref { arg } => Ok(self.expr_to_constraint(arg.clone(), env)?),
            // なんとかするとこ
            Borrow { arg } => Ok(self.expr_to_constraint(arg.clone(), env)?),
            _ => {
                println!("{}", env.get_assumptions()?);
                Err(AnalysisError::UnsupportedPattern(format!("name: {:?}", arg.kind)))
            }
        }
    }

    pub fn literal_to_constraint(lit: &'tcx Lit, neg: bool) -> Result<String, AnalysisError> {
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

    pub fn var_ref_to_constraint(
        &self, id: &LocalVarId, env: &Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        Ok(env
            .var_map
            .get(id)
            .expect("var not found in ver_ref_to_constraint")
            .assume
            .0
            .to_string())
    }

    pub fn logical_op_to_constraint(
        &self, op: LogicalOp, lhs_str: &String, rhs_str: &String,
    ) -> Result<String, AnalysisError> {
        use LogicalOp::*;

        let op_str = match op {
            And => "and",
            Or => "or",
        };
        Ok(format!("({} {} {})", op_str, lhs_str, rhs_str))
    }

    pub fn un_op_to_constraint(&self, op: UnOp, arg_str: &String) -> Result<String, AnalysisError> {
        use UnOp::*;

        let op_str = match op {
            Not => "not",
            Neg => "-",
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", op))),
        };
        Ok(format!("({} {})", op_str, arg_str))
    }

    pub fn bin_op_to_constraint(
        &self, op: BinOp, lhs_str: &String, rhs_str: &String,
    ) -> Result<String, AnalysisError> {
        Ok(format!("({} {} {})", Analyzer::bin_op_to_smt(op)?, lhs_str, rhs_str))
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
            _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", op))),
        };
        Ok(op_str.to_string())
    }
    pub fn fn_to_constraint(
        &self, ty: Ty<'tcx>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
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

    pub fn local_fn_to_constraint(
        &self, expr: Rc<RThir<'tcx>>, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        self.analyze_params(&expr.params, args, env)?;
        self.block_to_constraint(expr.body.as_ref().expect("Body not found").clone(), env)
    }

    pub fn extern_fn_to_constraint(
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

    pub fn if_to_constraint(
        &self, cond: Rc<RExpr<'tcx>>, then_block: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        let cond_str = self.expr_to_constraint(cond.clone(), env)?;

        let mut then_env = env.gen_new_env("then".to_string())?;
        then_env.add_assume(cond_str.clone());
        let then_value = self.block_to_constraint(then_block, &mut then_env)?;

        let else_block = else_opt.expect("Else block of if initializer not found");
        let mut else_env = env.gen_new_env("else".to_string())?;
        else_env.add_assume(format!("(not {})", cond_str.clone()));
        let else_value = self.block_to_constraint(else_block, &mut else_env)?;

        env.merge_then_else_env(cond_str.clone(), then_env, Some(else_env))?;
        Ok(Analyzer::value_to_ite(cond_str, then_value, else_value))
    }

    pub fn value_to_ite(cond_str: String, then_value: String, else_value: String) -> String {
        format!("(ite {} {} {})", cond_str, then_value, else_value)
    }

    pub fn block_to_constraint(
        &self, block: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<String, AnalysisError> {
        let mut return_value = String::new();
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = block.as_ref() {
            for stmt in stmts {
                if let AnalysisType::Return(value) = self.analyze_expr(stmt.clone(), env)? {
                    return Ok(value.expect("No value with return"));
                }
            }
            if let Some(expr) = expr {
                return_value = self.expr_to_constraint(expr.clone(), env)?;
            }
        } else {
            println!("{}", env.get_assumptions()?);
            println!("{:?}", block);
            return Err(AnalysisError::UnsupportedPattern("Unknown body pattern".into()));
        }
        Ok(return_value)
    }
}
