// rustc crates
use rustc_span::def_id::LocalDefId;

// std crates
use std::collections::{HashMap as Map, VecDeque as Queue};

// Own crates
use crate::thir::rthir::*;
mod analyzer;
mod lir;
use lir::Lir;

pub fn analyze<'tcx>(main_id: LocalDefId, fn_map: Map<LocalDefId, RThir<'tcx>>) {
    Analyzer::run(main_id, fn_map);
}

struct Analyzer<'tcx> {
    fn_map: Map<LocalDefId, RThir<'tcx>>,
    path_map: Map<(Lir, bool), Queue<Lir>>,
    current_path: Queue<Lir>,
}

impl<'tcx> Analyzer<'tcx> {
    pub fn new(fn_map: Map<LocalDefId, RThir<'tcx>>) -> Self {
        Self { fn_map, path_map: Map::new(), current_path: Queue::new() }
    }

    pub fn run(main_id: LocalDefId, fn_map: Map<LocalDefId, RThir<'tcx>>) {
        let mut analyzer = Analyzer::new(fn_map);
        let main = analyzer.get_fn(main_id);
        analyzer.analyze_body(main);
    }

    fn get_fn(&self, fn_id: LocalDefId) -> RThir<'tcx> {
        self.fn_map.get(&fn_id).expect("No function with the given id").clone()
    }

    fn analyze_body(&mut self, rthir: RThir<'tcx>) { self.analyze_params(&rthir.params); }

    fn analyze_params(&mut self, params: &Vec<RParam<'tcx>>) {
        for param in params {
            if let Some(param) = param {
                let declaretion = Lir::new_declaretion(param);
                self.current_path.push_back(declaretion);
            }
        }
    }
}
