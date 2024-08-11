// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_middle::ty::Ty;
use rustc_span::Span;
use std::rc::Rc;

// std crates
use std::collections::{HashMap as Map, VecDeque};

// Own crates
use crate::analyze::{lir::*, AnalysisError, Analyzer};
use crate::thir::rthir::*;

#[derive(Clone)]
pub struct Env<'tcx> {
    pub name: String,
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

    pub fn add_assumption(&mut self, constraint: String, expr: Rc<RExpr<'tcx>>) {
        let new_assumption = Lir::new_assume(constraint, expr);
        self.path.push_back(new_assumption);
    }

    pub fn add_parameter(
        &mut self, name: String, ty: &Ty<'tcx>, var_id: &LocalVarId, pat: Rc<RExpr<'tcx>>,
    ) {
        let parameter = Lir::new_parameter(name.to_string(), ty.clone(), pat);
        self.path.push_back(parameter);
        self.insert_var(var_id, name, ty);
    }

    pub fn add_lir(&mut self, lir: Lir<'tcx>) { self.path.push_back(lir) }

    pub fn assign_new_value(
        &mut self, expr: Rc<RExpr<'tcx>>,
    ) -> Result<(String, String), AnalysisError> {
        match expr.kind {
            RExprKind::VarRef { id } => {
                let (current_symbol, ty) = self.get_var(&id);
                let new_symbol = Analyzer::span_to_str(&expr.span);
                let new_parameter =
                    Lir::new_parameter(new_symbol.clone(), ty.clone(), expr.clone());
                self.add_lir(new_parameter);
                self.insert_var(&id, new_symbol.clone(), &ty);
                Ok((new_symbol, current_symbol))
            }
            _ => unreachable!(),
        }
    }

    pub fn new_env_name(&self, name: &str) -> String {
        if self.name.starts_with(name) {
            format!("{}+", self.name)
        } else {
            name.to_string()
        }
    }

    pub fn insert_var(&mut self, var_id: &LocalVarId, name: String, ty: &Ty<'tcx>) {
        self.var_map.insert(var_id.clone(), (name, ty.clone()));
    }

    pub fn get_var(&self, var_id: &LocalVarId) -> (String, Ty<'tcx>) {
        self.var_map.get(var_id).unwrap().clone()
    }

    pub fn merge_env(&mut self, env: Env<'tcx>) {
        for assumption in env.path.iter() {
            match assumption.kind {
                LirKind::Declaration { .. } => self.add_lir(assumption.clone()),
                LirKind::Assume(_) => self.add_lir(assumption.clone()),
                _ => (),
            }
        }

        let mut new_var_map = Map::new();
        let current_var_map = self.var_map.clone();
        for (var_id, (var_str, ty)) in current_var_map.iter() {
            if let Some((env_var_str, ..)) = env.var_map.get(var_id) {
                if var_str != env_var_str {
                    let new_var_str = format!("{}+", var_str);
                    let last = env.path.back().expect("No lir found in new_var_map").expr.clone();
                    self.add_lir(Lir::new_parameter(new_var_str.clone(), ty.clone(), last.clone()));
                    self.add_lir(Lir::new_assume(
                        format!("(= {} {})", new_var_str, var_str),
                        last.clone(),
                    ));
                    self.add_lir(Lir::new_assume(
                        format!("(= {} {})", new_var_str, env_var_str),
                        last.clone(),
                    ));
                    new_var_map.insert(var_id.clone(), (new_var_str, ty.clone()));
                } else {
                    new_var_map.insert(var_id.clone(), (var_str.clone(), ty.clone()));
                }
            }
        }
        self.var_map = new_var_map;
    }

    pub fn gen_new_env(
        &self, name: String, expr: Rc<RExpr<'tcx>>,
    ) -> Result<Env<'tcx>, AnalysisError> {
        let name = self.new_env_name(&name);
        let assumptions = Lir::new_assumptions(self.get_assumptions()?, expr);
        let mut new_path = VecDeque::new();
        new_path.push_back(assumptions);
        Ok(Env::from(name, new_path, self.var_map.clone()))
    }

    pub fn merge_then_else_env(
        &mut self, cond_str: String, mut then_env: Env<'tcx>, else_env: Option<Env<'tcx>>,
    ) -> Result<(), AnalysisError> {
        then_env.adapt_cond_to_path(&cond_str)?;
        self.merge_env(then_env);
        if let Some(mut else_env) = else_env {
            else_env.adapt_cond_to_path(&format!("(not {})", cond_str))?;
            self.merge_env(else_env);
        }
        Ok(())
    }

    fn adapt_cond_to_path(&mut self, cond_str: &String) -> Result<(), AnalysisError> {
        let mut adapted_path = VecDeque::new();
        for lir in self.path.iter() {
            match &lir.kind {
                LirKind::Declaration { .. } => adapted_path.push_back(lir.clone()),
                LirKind::Assume(constraint) => {
                    if constraint != cond_str {
                        adapted_path.push_back(Lir::new_assume(
                            format!("(=> {} {})", cond_str, constraint),
                            lir.expr.clone(),
                        ));
                    }
                }
                _ => (),
            }
        }
        self.path = adapted_path;
        Ok(())
    }
}
