// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_span::{def_id::LocalDefId, Span};

// std crates
use std::collections::{HashMap as Map, VecDeque};
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;
mod lir;
use lir::Lir;

pub fn analyze<'tcx>(
    main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
) -> Result<(), AnalysisError> {
    Analyzer::run(main_id, fn_map)
}

#[derive(Debug)]
struct Analyzer<'tcx> {
    fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    path_map: Map<(Lir<'tcx>, bool), VecDeque<Lir<'tcx>>>,
    current_path: VecDeque<Lir<'tcx>>,
    var_map: Map<LocalVarId, Rc<RExpr<'tcx>>>,
}

#[derive(Debug)]
pub enum AnalysisError {
    FunctionNotFound(LocalDefId),
    UnsupportedPattern(String),
    VerifyError { message: String, span: Span },
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>) -> Self {
        Self { fn_map, path_map: Map::new(), current_path: VecDeque::new(), var_map: Map::new() }
    }

    pub fn run(
        main_id: LocalDefId, fn_map: Map<LocalDefId, Rc<RThir<'tcx>>>,
    ) -> Result<(), AnalysisError> {
        let mut analyzer = Analyzer::new(fn_map);
        let main = analyzer.get_fn(main_id)?;
        analyzer.analyze_fn(main)
    }

    fn get_fn(&self, fn_id: LocalDefId) -> Result<Rc<RThir<'tcx>>, AnalysisError> {
        self.fn_map.get(&fn_id).cloned().ok_or(AnalysisError::FunctionNotFound(fn_id))
    }

    fn analyze_fn(&mut self, rthir: Rc<RThir<'tcx>>) -> Result<(), AnalysisError> {
        self.analyze_params(&rthir.params)?;
        if let Some(body) = &rthir.body {
            self.analyze_body((*body).clone())?;
        }
        Ok(())
    }

    fn analyze_params(&mut self, params: &[RParam<'tcx>]) -> Result<(), AnalysisError> {
        use RExprKind::*;
        use RPatKind::*;

        for param in params {
            if let Some(pat) = &param.pat {
                if let RExpr { kind: Pat { kind }, .. } = &**pat {
                    match kind {
                        Binding { name, ty, var, .. } => {
                            let parameter =
                                Lir::<'tcx>::new_parameter(name.clone(), ty.clone(), pat.clone());
                            self.current_path.push_back(parameter);
                            self.var_map.insert(var.clone(), pat.clone());
                        }
                        _ => return Err(AnalysisError::UnsupportedPattern(format!("{:?}", kind))),
                    }
                }
            }
        }
        Ok(())
    }

    fn analyze_body(&mut self, body: Rc<RExpr<'tcx>>) -> Result<(), AnalysisError> { Ok(()) }
}
