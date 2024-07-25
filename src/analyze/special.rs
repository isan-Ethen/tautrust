// rustc crates
// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn analyze_t3assert(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        self.analyze_t3assume(args, env)?;
        let smt = env.get_assumptions_for_verify()?;
        self.verify(smt, env)?;
        Ok(AnalysisType::Other)
    }

    pub fn analyze_t3assume(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>, env: &mut Env<'tcx>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        let constraint = self.expr_to_constraint(args[0].clone(), env)?;
        env.add_assumption(constraint, args[0].clone());
        Ok(AnalysisType::Other)
    }

    pub fn analyze_invariant(
        &mut self, args: Box<[Rc<RExpr<'tcx>>]>,
    ) -> Result<AnalysisType<'tcx>, AnalysisError> {
        Ok(AnalysisType::Invariant(Vec::from_iter(args.iter()).remove(0).clone()))
    }
}
