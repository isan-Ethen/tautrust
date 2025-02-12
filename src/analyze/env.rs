// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_middle::ty::TyKind;
use rustc_span::Span;
use std::rc::Rc;

// std crates
use std::collections::HashMap as Map;
use std::io::Write;
use std::process::Command;

// Own crates
use crate::analyze::{lir::*, AnalysisError, Analyzer};
use crate::thir::rthir::*;

#[derive(Clone)]
pub struct Env<'tcx> {
    pub smt_vars: Vec<(String, TyKind<'tcx>)>,
    pub path: Vec<String>,
    pub var_map: Map<LocalVarId, Lir<'tcx>>,
}

impl<'tcx> Env<'tcx> {
    pub fn new() -> Self { Self { smt_vars: Vec::new(), path: Vec::new(), var_map: Map::new() } }

    pub fn from(
        smt_vars: Vec<(String, TyKind<'tcx>)>, path: Vec<String>,
        var_map: Map<LocalVarId, Lir<'tcx>>,
    ) -> Self {
        Self { smt_vars, path, var_map }
    }

    pub fn verify(&mut self, constraint: &String, span: Span) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .args(["-in", "-model"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut smt = String::new();
        smt.push_str(&self.get_assumptions()?);
        smt.push_str(&format!("(assert (not {constraint}))\n"));

        let mut stdin = child.stdin.take().expect("Open stdin failed");
        smt += "(check-sat)\n";
        println!("{smt}");
        stdin.write_all(smt.as_bytes()).expect("Write smt failed");
        drop(stdin);

        let output = child.wait_with_output().expect("Get stdout failed");
        let result = String::from_utf8(output.stdout).expect("Load result failed");
        if &result != "unsat\n" {
            return Err(AnalysisError::VerifyError(span));
        }

        println!("Verification success!\n");

        Ok(())
    }

    pub fn len(&self) -> usize { self.path.len() }

    pub fn add_assume(&mut self, constraint: String) { self.path.push(constraint); }

    pub fn get_assumptions(&self) -> Result<String, AnalysisError> {
        let mut smt = String::new();
        for smt_var in self.smt_vars.iter() {
            smt.push_str(&self.to_smt(smt_var)?);
        }
        for constraint in self.path.iter() {
            smt.push_str(&format!("(assert {constraint})\n"));
        }
        Ok(smt)
    }

    pub fn to_smt(&self, var: &(String, TyKind<'tcx>)) -> Result<String, AnalysisError> {
        match var.1 {
            TyKind::Bool => Ok(format!("(declare-const {} Bool)\n", var.0)),
            TyKind::Int(_) => Ok(format!("(declare-const {} Int)\n", var.0)),
            TyKind::Float(_) => Ok(format!("(declare-const {} Real)\n", var.0)),
            TyKind::Ref(_, ty, _) => {
                Ok(format!("(declare-const {} {})\n", var.0, Env::type_to_str(&ty.kind())?,))
            }
            _ => Err(AnalysisError::UnsupportedPattern(format!(
                "Unknown TyKind: {:?} in to_smt",
                var.1
            ))),
        }
    }

    pub fn type_to_str(ty: &TyKind<'tcx>) -> Result<String, AnalysisError> {
        match ty {
            TyKind::Bool => Ok("Bool".into()),
            TyKind::Int(_) => Ok("Int".into()),
            TyKind::Float(_) => Ok("Real".into()),
            _ => Err(AnalysisError::UnsupportedPattern(format!(
                "Unknown TyKind: {:?} in type_to_str",
                ty
            ))),
        }
    }

    pub fn add_assumption(
        &mut self, var_id: &LocalVarId, operation: String, arg: String, expr: Rc<RExpr<'tcx>>,
    ) {
        let var = self.var_map.get_mut(var_id).expect("Variable not found");
        var.adapt_var_expr(&operation, &arg, expr);
    }

    pub fn add_parameter(&mut self, ty: &TyKind<'tcx>, var_id: &LocalVarId, pat: Rc<RExpr<'tcx>>) {
        self.var_map.insert(*var_id, Lir::new(*ty, vec![String::new()], pat).unwrap());
    }

    pub fn add_mutable_ref(&mut self, var_id: &LocalVarId, lir: Lir<'tcx>) {
        self.var_map.insert(*var_id, lir);
    }

    pub fn add_rand(&mut self, name: String, ty: &TyKind<'tcx>) { self.smt_vars.push((name, *ty)); }

    pub fn assign_new_value(&mut self, target_id: &LocalVarId, constraint: String) {
        let target = self.var_map.get_mut(target_id).expect("target value not found");
        target.set_var_expr(constraint);
    }

    pub fn assign_var_expr(&mut self, target_id: &LocalVarId, var_expr: LirKind<'tcx>) {
        let target = self.var_map.get_mut(target_id).expect("target value not found");
        target.kind = var_expr;
    }

    pub fn merge_then_else_env(
        &mut self, cond_str: String, mut then_env: Env<'tcx>, mut else_env: Option<Env<'tcx>>,
    ) -> Result<(), AnalysisError> {
        let len = self.len();
        then_env.adapt_cond_to_path(&cond_str, len);
        if let Some(else_env) = &mut else_env {
            else_env.adapt_cond_to_path(&format!("(not {})", cond_str), len);
        }
        self.merge_env(cond_str, then_env, else_env);
        Ok(())
    }

    fn adapt_cond_to_path(&mut self, cond_str: &str, len: usize) {
        for assume in &mut self.path[len..] {
            *assume = format!("(=> {cond_str} {assume})");
        }
    }

    fn merge_env(&mut self, cond: String, then_env: Env<'tcx>, else_env: Option<Env<'tcx>>) {
        self.extend_smt_vars(self.smt_vars.len(), &then_env, &else_env);
        self.extend_paths(self.path.len(), &then_env, &else_env);
        self.merge_var_maps(cond, then_env, else_env);
    }

    fn extend_smt_vars(&mut self, len: usize, then_env: &Env<'tcx>, else_env: &Option<Env<'tcx>>) {
        self.smt_vars.extend_from_slice(&then_env.smt_vars[len..]);
        if let Some(else_env) = else_env {
            self.smt_vars.extend_from_slice(&else_env.smt_vars[len..]);
        }
    }

    fn extend_paths(&mut self, len: usize, then_env: &Env<'tcx>, else_env: &Option<Env<'tcx>>) {
        self.path.extend_from_slice(&then_env.path[len + 1..]);
        if let Some(else_env) = else_env {
            self.path.extend_from_slice(&else_env.path[len + 1..]);
        }
    }

    fn merge_var_maps(&mut self, cond: String, then_env: Env<'tcx>, else_env: Option<Env<'tcx>>) {
        let mut new_var_map = Map::new();

        for (var_id, current_lir) in self.var_map.iter_mut() {
            Env::merge_lir(&var_id, &cond, current_lir, &then_env, &else_env);
            new_var_map.insert(*var_id, current_lir.clone());
        }

        self.var_map = new_var_map;
    }

    fn merge_lir(
        var_id: &LocalVarId, cond: &String, current_lir: &mut Lir<'tcx>, then_env: &Env<'tcx>,
        else_env: &Option<Env<'tcx>>,
    ) {
        match (then_env.var_map.get(var_id), else_env.as_ref().and_then(|e| e.var_map.get(var_id)))
        {
            (Some(then_lir), Some(else_lir)) => {
                Env::merge_both_branches(cond, current_lir, then_lir, else_lir);
            }
            (Some(then_lir), None) => Env::merge_then_branch(cond, current_lir, then_lir),
            _ => unreachable!("then and else env not found"),
        }
    }

    fn merge_both_branches(
        cond: &String, current_lir: &mut Lir<'tcx>, then_lir: &Lir<'tcx>, else_lir: &Lir<'tcx>,
    ) {
        if current_lir.get_var_expr() == then_lir.get_var_expr()
            && current_lir.get_var_expr() == else_lir.get_var_expr()
        {
            return;
        }

        let var_expr =
            Analyzer::value_to_ite(cond, then_lir.get_var_expr(), else_lir.get_var_expr());

        current_lir.set_var_expr(var_expr);
    }

    fn merge_then_branch(cond: &String, current_lir: &mut Lir<'tcx>, then_lir: &Lir<'tcx>) {
        if current_lir.get_var_expr() == then_lir.get_var_expr() {
            return;
        }

        let var_expr =
            Analyzer::value_to_ite(cond, then_lir.get_var_expr(), current_lir.get_var_expr());

        current_lir.set_var_expr(var_expr);
    }
}
