// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    // pub fn analyze_t3assert(
    //     &self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    // ) -> Result<AnalysisType<'tcx>, AnalysisError> {
    //     let constraint = self.expr_to_constraint(args[0].clone(), env)?;
    //     env.verify(constraint.get_assume(), args[0].span)?;
    //     Ok(AnalysisType::Other)
    // }

    // pub fn analyze_t3assume(
    //     &self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    // ) -> Result<AnalysisType<'tcx>, AnalysisError> {
    //     let constraint = self.expr_to_constraint(args[0].clone(), env)?;
    //     env.add_assume(constraint.get_assume().into());
    //     Ok(AnalysisType::Other)
    // }

    // pub fn analyze_invariant(
    //     &self, args: Box<[Rc<RExpr<'tcx>>]>,
    // ) -> Result<AnalysisType<'tcx>, AnalysisError> {
    //     Ok(AnalysisType::Invariant(Vec::from_iter(args.iter()).remove(0).clone()))
    // }

    // pub fn analyze_drop(
    //     &self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    // ) -> Result<AnalysisType<'tcx>, AnalysisError> {
    //     if let RExprKind::VarRef { id } = args[0].kind {
    //         let arg = env.var_map.get(&id).expect("Var not found in t3drop");
    //         env.add_assume(format!("(= {} {})", arg.get_assume(), arg.get_assume_by_index(vec![1])))
    //     }
    //     Ok(AnalysisType::Other)
    // }
}
