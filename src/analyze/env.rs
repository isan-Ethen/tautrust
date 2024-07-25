// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_middle::ty::Ty;
use rustc_span::Span;

// std crates
use std::collections::{HashMap as Map, VecDeque};

// Own crates
use crate::analyze::{AnalysisError, Lir};

#[derive(Clone)]
pub struct Env<'tcx> {
    name: String,
    pub path: VecDeque<Lir<'tcx>>,
    pub var_map: Map<LocalVarId, (String, Ty<'tcx>)>,
}

impl<'tcx> Env<'tcx> {
    pub fn new() -> Self {
        Self { name: "main".to_string(), path: VecDeque::new(), var_map: Map::new() }
    }

    pub fn from(
        name: String, path: VecDeque<Lir<'tcx>>, var_map: Map<LocalVarId, (String, Ty<'tcx>)>,
    ) -> Self {
        Self { name, path, var_map }
    }

    pub fn name(&self) -> String { self.name.clone() }

    pub fn len(&self) -> usize { self.path.len() }

    pub fn get_assumptions_for_verify(&self) -> Result<String, AnalysisError> {
        let mut smt = String::new();
        let len = self.len();
        let mut cnt = 0;
        loop {
            if len - 1 == cnt {
                smt.push_str(&self.get_assumption(cnt).to_assert());
                break;
            }
            smt.push_str(&self.get_assumption(cnt).to_smt()?);
            cnt += 1;
        }
        Ok(smt)
    }

    pub fn get_assumptions(&self) -> Result<String, AnalysisError> {
        let mut smt = String::new();
        for assumption in self.path.iter() {
            smt.push_str(&assumption.to_smt()?);
        }
        Ok(smt)
    }

    pub fn get_assumption(&self, index: usize) -> &Lir<'tcx> { &self.path[index] }

    pub fn get_latest_span(&self) -> Span { self.path.back().expect("Lir not fhound").get_span() }

    pub fn add_assumption(&mut self, lir: Lir<'tcx>) { self.path.push_back(lir); }

    pub fn insert_var(&mut self, var_id: &LocalVarId, name: String, ty: &Ty<'tcx>) {
        self.var_map.insert(var_id.clone(), (name, ty.clone()));
    }

    pub fn get_var(&self, var_id: &LocalVarId) -> (String, Ty<'tcx>) {
        self.var_map.get(var_id).unwrap().clone()
    }
}
