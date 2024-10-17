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
use crate::analyze::{
    lir::*,
    AnalysisError, //Analyzer
};
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

    pub fn verify(&mut self, assert: &String, span: Span) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .args(["-in", "-model"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut smt = String::new();
        smt.push_str(&self.get_assumptions()?);
        smt.push_str(&format!("(assert (not {assert}))\n"));

        let mut stdin = child.stdin.take().expect("Open std failed");
        smt += "(check-sat)\n";
        println!("{smt}");
        stdin.write_all(smt.as_bytes()).expect("Write smt failed");
        drop(stdin);

        let output = child.wait_with_output().expect("Get stdout failed");
        let result = String::from_utf8(output.stdout).expect("Load result failed");
        if &result != "unsat\n" {
            return Err(AnalysisError::VerifyError { span });
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
        var.adapt_assume(&operation, &arg, expr);
    }

    pub fn add_parameter(&mut self, ty: &TyKind<'tcx>, var_id: &LocalVarId, pat: Rc<RExpr<'tcx>>) {
        self.var_map.insert(*var_id, Lir::new(*ty, vec![String::new()], pat).unwrap());
    }

    pub fn add_mutable_ref(&mut self, var_id: &LocalVarId, lir: Lir<'tcx>) {
        self.var_map.insert(*var_id, lir);
    }

    pub fn add_rand(&mut self, name: String, ty: &TyKind<'tcx>) { self.smt_vars.push((name, *ty)); }

    pub fn assign_new_value(&mut self, target_id: &LocalVarId, constraint: String) {
        let target = self.var_map.get_mut(target_id).expect("target not found");
        target.set_assume(constraint);
    }

    pub fn assign_assume(&mut self, target_id: &LocalVarId, assume: LirKind<'tcx>) {
        let target = self.var_map.get_mut(target_id).expect("target not found");
        target.kind = assume;
    }

    // pub fn merge_env(&mut self, cond: String, then_env: Env<'tcx>, else_env: Option<Env<'tcx>>) {
    //     let len = self.len();
    //     self.path.extend_from_slice(&then_env.path[len + 1..]);

    //     if let Some(ref else_env) = else_env {
    //         self.path.extend_from_slice(&else_env.path[len + 1..]);
    //     }
    //     let mut new_var_map = Map::new();
    //     let current_var_map = self.var_map.clone();

    //     for (var_id, current_lir) in current_var_map {
    //         let new_lir = match (
    //             then_env.var_map.get(&var_id),
    //             else_env.as_ref().and_then(|e| e.var_map.get(&var_id)),
    //         ) {
    //             (Some(then_lir), Some(else_lir)) => {
    //                 let assume = vec![Analyzer::value_to_ite(
    //                     &cond,
    //                     then_lir.get_assume(),
    //                     else_lir.get_assume(),
    //                 )];
    //                 if then_lir.get_assume() != else_lir.get_assume() {
    //                     Lir::new(current_lir.get_ty(), assume, current_lir.expr.clone())
    //                         .expect("failed to make lir")
    //                 } else {
    //                     current_lir
    //                 }
    //             }
    //             (Some(then_lir), None) => {
    //                 if current_lir.get_assume() != then_lir.get_assume() {
    //                     let assume = Analyzer::value_to_ite(
    //                         &cond,
    //                         then_lir.get_assume(),
    //                         current_lir.get_assume(),
    //                     );
    //                     let mut updated_lir = current_lir;
    //                     updated_lir.set_assume(assume);
    //                     updated_lir
    //                 } else {
    //                     current_lir
    //                 }
    //             }
    //             _ => current_lir,
    //         };

    //         new_var_map.insert(var_id, new_lir);
    //     }

    //     self.var_map = new_var_map;
    // }

    // pub fn gen_new_env(&self, name: String) -> Result<Env<'tcx>, AnalysisError> {
    //     let name = self.new_env_name(&name);
    //     Ok(Env::from(name, self.smt_vars.clone(), self.path.clone(), self.var_map.clone()))
    // }

    // pub fn merge_then_else_env(
    //     &mut self, cond_str: String, mut then_env: Env<'tcx>, mut else_env: Option<Env<'tcx>>,
    // ) -> Result<(), AnalysisError> {
    //     let len = self.len();
    //     then_env.adapt_cond_to_path(&cond_str, &len)?;
    //     if let Some(else_env) = &mut else_env {
    //         else_env.adapt_cond_to_path(&format!("(not {})", cond_str), &len)?;
    //     }
    //     self.merge_env(cond_str, then_env, else_env);
    //     Ok(())
    // }

    // fn adapt_cond_to_path(&mut self, cond_str: &String, len: &usize) -> Result<(), AnalysisError> {
    //     for i in *len..self.len() {
    //         self.path[i] = format!("(=> {cond_str} {})", self.path[i]);
    //     }
    //     Ok(())
    // }
}
