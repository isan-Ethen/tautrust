// rustc crates
use rustc_middle::thir::LocalVarId;
use rustc_middle::ty::{Ty, TyKind};
use rustc_span::Span;
use std::rc::Rc;

// std crates
use std::collections::{HashMap as Map, VecDeque};
use std::io::Write;
use std::process::Command;

// Own crates
use crate::analyze::{lir::*, AnalysisError, Analyzer};
use crate::thir::rthir::*;

#[derive(Clone)]
pub struct Env<'tcx> {
    pub name: String,
    pub smt_vars: Vec<(String, Ty<'tcx>)>,
    pub path: Vec<String>,
    pub var_map: Map<LocalVarId, Lir<'tcx>>,
}

impl<'tcx> Env<'tcx> {
    pub fn new() -> Self {
        Self {
            name: "main".to_string(),
            smt_vars: Vec::new(),
            path: Vec::new(),
            var_map: Map::new(),
        }
    }

    pub fn from(
        name: String, smt_vars: Vec<(String, Ty<'tcx>)>, path: Vec<String>,
        var_map: Map<LocalVarId, Lir<'tcx>>,
    ) -> Self {
        Self { name, smt_vars, path, var_map }
    }

    pub fn verify(&mut self, assert: String, span: Span) -> Result<(), AnalysisError> {
        let mut child = Command::new("z3")
            .args(["-in", "-model"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Run z3 failed");

        let mut smt = self.get_assumptions()?;
        smt.push_str(&format!("(assert (not {}))\n", assert));

        let mut stdin = child.stdin.take().expect("Open std failed");
        smt += "(check-sat)\n";
        println!("{}", smt);
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

    pub fn len(&self) -> usize { self.smt_vars.len() }

    pub fn add_assume(&mut self, constraint: String) { self.path.push(constraint); }

    pub fn get_assumptions(&self) -> Result<String, AnalysisError> {
        let mut smt = String::new();
        for smt_var in self.smt_vars.iter() {
            smt.push_str(&self.to_smt(smt_var)?);
        }
        for constraint in self.path.iter() {
            smt.push_str(&format!("(assert {})\n", constraint));
        }
        Ok(smt)
    }

    pub fn to_smt(&self, var: &(String, Ty<'tcx>)) -> Result<String, AnalysisError> {
        match var.1.kind() {
            TyKind::Bool => Ok(format!("(declare-const {} Bool)\n", var.0)),
            TyKind::Int(_) => Ok(format!("(declare-const {} Int)\n", var.0)),
            TyKind::Float(_) => Ok(format!("(declare-const {} Real)\n", var.0)),
            _ => Err(AnalysisError::UnsupportedPattern("Unknown TyKind".into())),
        }
    }

    pub fn add_assumption(
        &mut self, var_id: &LocalVarId, operation: String, arg: String, expr: Rc<RExpr<'tcx>>,
    ) {
        let var = self.var_map.get_mut(var_id).expect("Variable not found");
        var.adapt_assume(operation, arg, expr);
    }

    pub fn add_parameter(
        &mut self, name: String, ty: &Ty<'tcx>, var_id: &LocalVarId, pat: Rc<RExpr<'tcx>>,
    ) {
        self.var_map.insert(var_id.clone(), Lir::new(name, ty.clone(), String::new(), pat));
    }

    pub fn add_rand(&mut self, name: String, ty: &Ty<'tcx>) {
        self.smt_vars.push((name, ty.clone()));
    }

    pub fn assign_new_value(&mut self, target_id: &LocalVarId, constraint: String) {
        let target = self.var_map.get_mut(target_id).expect("target not found");
        target.assume = constraint;
    }

    pub fn new_env_name(&self, name: &str) -> String {
        if self.name.starts_with(name) {
            format!("{}+", self.name)
        } else {
            name.to_string()
        }
    }

    // pub fn merge_env(&mut self, env: Env<'tcx>) {
    //     for assumption in env.smt_vars.iter() {
    //         match assumption.kind {
    //             LirKind::Declaration { .. } => self.add_lir(assumption.clone()),
    //             LirKind::Assume(_) => self.add_lir(assumption.clone()),
    //             _ => (),
    //         }
    //     }

    //     let mut new_var_map = Map::new();
    //     let current_var_map = self.var_map.clone();
    //     for (var_id, (var_str, ty)) in current_var_map.iter() {
    //         if let Some((env_var_str, ..)) = env.var_map.get(var_id) {
    //             if var_str != env_var_str {
    //                 let new_var_str = format!("{}+", var_str);
    //                 let last =
    //                     env.smt_vars.back().expect("No lir found in new_var_map").expr.clone();
    //                 self.add_lir(Lir::new_parameter(new_var_str.clone(), ty.clone(), last.clone()));
    //                 self.add_lir(Lir::new_assume(
    //                     format!("(= {} {})", new_var_str, var_str),
    //                     last.clone(),
    //                 ));
    //                 self.add_lir(Lir::new_assume(
    //                     format!("(= {} {})", new_var_str, env_var_str),
    //                     last.clone(),
    //                 ));
    //                 new_var_map.insert(var_id.clone(), (new_var_str, ty.clone()));
    //             } else {
    //                 new_var_map.insert(var_id.clone(), (var_str.clone(), ty.clone()));
    //             }
    //         }
    //     }
    //     self.var_map = new_var_map;
    // }

    pub fn gen_new_env(&self, name: String) -> Result<Env<'tcx>, AnalysisError> {
        let name = self.new_env_name(&name);
        Ok(Env::from(name, self.smt_vars.clone(), self.path.clone(), self.var_map.clone()))
    }

    // pub fn merge_then_else_env(
    //     &mut self, cond_str: String, mut then_env: Env<'tcx>, else_env: Option<Env<'tcx>>,
    // ) -> Result<(), AnalysisError> {
    //     then_env.adapt_cond_to_smt_vars(&cond_str)?;
    //     self.merge_env(then_env);
    //     if let Some(mut else_env) = else_env {
    //         else_env.adapt_cond_to_smt_vars(&format!("(not {})", cond_str))?;
    //         self.merge_env(else_env);
    //     }
    //     Ok(())
    // }

    // fn adapt_cond_to_path(&mut self, cond_str: &String) -> Result<(), AnalysisError> {
    //     let mut adapted_smt_vars = VecDeque::new();
    //     for lir in self.smt_vars.iter() {
    //         match &lir.kind {
    //             LirKind::Declaration { .. } => adapted_smt_vars.push_back(lir.clone()),
    //             LirKind::Assume(constraint) => {
    //                 if constraint != cond_str {
    //                     adapted_smt_vars.push_back(Lir::new_assume(
    //                         format!("(=> {} {})", cond_str, constraint),
    //                         lir.expr.clone(),
    //                     ));
    //                 }
    //             }
    //             _ => (),
    //         }
    //     }
    //     self.smt_vars = adapted_path;
    //     Ok(())
    // }
}
