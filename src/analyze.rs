// rustc crates
use rustc_ast::ast::LitKind;
use rustc_hir::Lit;
use rustc_middle::mir::{BinOp, UnOp};
use rustc_middle::thir::LocalVarId;
use rustc_middle::thir::LogicalOp;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::{Ty, TyKind};
use rustc_span::def_id::LocalDefId;

// std crates
use std::boxed::Box;
use std::collections::HashMap as Map;
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;
mod lir;
pub use lir::*;
mod helper_struct;
pub use helper_struct::*;
mod env;
pub use env::Env;
mod core;
mod gen_cstr;
mod special;
mod sub;
mod util;

pub fn analyze<'tcx>(
    main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>,
) -> Result<(), AnalysisError> {
    Analyzer::run(main_id, fn_map, tcx)
}

struct Analyzer<'tcx> {
    fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>) -> Self {
        Self { fn_map, tcx }
    }

    pub fn run(
        main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>, tcx: TyCtxt<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut analyzer = Analyzer::new(fn_map, tcx);
        let main = analyzer.get_fn(main_id)?;
        analyzer.analyze_main(main)
    }

    /// Main analysis functions
    /// - analyze_loop

    fn analyze_loop(
        &mut self, invariant: Rc<RExpr<'tcx>>, expr: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut loop_env = env.gen_new_env("loop".to_string(), expr.clone())?;
        let constraint = self.expr_to_constraint(invariant.clone(), &mut loop_env)?;
        self.verify_before_loop(&constraint, env)?;
        if let RExprKind::Loop { body } = expr.kind.clone() {
            loop_env.gen_new_env("inner_loop".to_string(), expr.clone())?;
            self.verify_inner_loop(constraint, invariant, body.clone(), env)?;
        } else {
            return Err(AnalysisError::UnsupportedPattern(
                "Multiple invariant is not suppoerted".into(),
            ));
        }
        Ok(())
    }

    fn verify_before_loop(
        &self, constraint: &String, env: &Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        let mut smt = env.get_assumptions()?;
        smt += &format!("(assert (not {}))\n", constraint);
        self.verify(smt, env)
    }

    fn verify_inner_loop(
        &mut self, constraint: String, invariant: Rc<RExpr<'tcx>>, block: Rc<RExpr<'tcx>>,
        env: &mut Env<'tcx>,
    ) -> Result<(), AnalysisError> {
        self.set_var_map(block.clone(), invariant.clone(), env);
        env.add_assumption(constraint, invariant.clone());
        let smt = env.get_assumptions_for_verify()?;
        println!("{}", smt);
        self.analyze_block(block, env)?;
        let smt = env.get_assumptions_for_verify()?;
        println!("{}", smt);
        self.verify(smt, env)
    }

    fn set_var_map(
        &mut self, block: Rc<RExpr<'tcx>>, invariant: Rc<RExpr<'tcx>>, env: &mut Env<'tcx>,
    ) {
        let inv_varv = Analyzer::search_inv(invariant);
        let varv = Analyzer::search_used_var(block.clone());
        let refresh_varv = varv.iter().filter(|var| !inv_varv.contains(var));
        for var in refresh_varv {
            let (current_name, ty) = env.var_map.get(var).unwrap().clone();
            let new_name = format!("{}_{}", env.name, current_name);
            env.add_parameter(new_name.clone(), &ty, var, block.clone());
            env.var_map.insert(*var, (new_name, ty.clone()));
        }
    }

    fn search_inv(invariant: Rc<RExpr<'tcx>>) -> Vec<LocalVarId> {
        let mut varv: Vec<LocalVarId> = Vec::new();
        Analyzer::search_var(invariant, &mut varv);
        varv
    }

    fn search_var(expr: Rc<RExpr<'tcx>>, varv: &mut Vec<LocalVarId>) {
        use RExprKind::*;

        match expr.kind.clone() {
            VarRef { id } => {
                varv.push(id.clone());
            }
            LogicalOp { lhs, rhs, .. } => {
                Analyzer::search_var(lhs.clone(), varv);
                Analyzer::search_var(rhs.clone(), varv);
            }
            Unary { arg, .. } => {
                Analyzer::search_var(arg.clone(), varv);
            }
            Binary { lhs, rhs, .. } => {
                Analyzer::search_var(lhs.clone(), varv);
                Analyzer::search_var(rhs.clone(), varv);
            }
            _ => panic!("Unknown invariant pattern"),
        }
    }

    fn search_used_var(block: Rc<RExpr<'tcx>>) -> Vec<LocalVarId> {
        let mut varv: Vec<LocalVarId> = Vec::new();
        if let RExpr { kind: RExprKind::Block { stmts, expr }, .. } = &*block {
            for stmt in stmts {
                Analyzer::search_var_expr(stmt.clone(), &mut varv, false);
            }
            if let Some(expr) = expr {
                Analyzer::search_var_expr(expr.clone(), &mut varv, false);
            }
        }
        varv
    }

    fn search_var_expr(expr: Rc<RExpr<'tcx>>, varv: &mut Vec<LocalVarId>, is_assign: bool) {
        use RExprKind::*;

        match &expr.kind {
            Literal { .. } => (),
            VarRef { id } => {
                if is_assign {
                    varv.push(id.clone());
                }
            }
            LogicalOp { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, is_assign);
                Analyzer::search_var_expr(rhs.clone(), varv, is_assign);
            }
            Unary { arg, .. } => {
                Analyzer::search_var_expr(arg.clone(), varv, is_assign);
            }
            Binary { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, is_assign);
                Analyzer::search_var_expr(rhs.clone(), varv, is_assign);
            }
            Call { .. } => (),
            If { then, else_opt, .. } => {
                Analyzer::search_var_expr(then.clone(), varv, is_assign);
                if let Some(else_block) = else_opt {
                    Analyzer::search_var_expr(else_block.clone(), varv, is_assign);
                }
            }
            LetStmt { initializer, .. } => {
                if let Some(initializer) = initializer {
                    Analyzer::search_var_expr(initializer.clone(), varv, is_assign);
                }
            }
            AssignOp { lhs, rhs, .. } => {
                Analyzer::search_var_expr(lhs.clone(), varv, true);
                Analyzer::search_var_expr(rhs.clone(), varv, false);
            }
            Assign { lhs, rhs } => {
                Analyzer::search_var_expr(lhs.clone(), varv, true);
                Analyzer::search_var_expr(rhs.clone(), varv, false);
            }
            Block { stmts, expr } => {
                for stmt in stmts {
                    Analyzer::search_var_expr(stmt.clone(), varv, is_assign);
                }
                if let Some(expr) = expr {
                    Analyzer::search_var_expr(expr.clone(), varv, false);
                }
            }
            // Break { .. } => (),
            _ => panic!("Unknown pattern in loop: {:?}", expr),
        }
    }
}
