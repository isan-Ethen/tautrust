// rustc crates
use rustc_middle::ty::{Ty, TyKind};
use rustc_span::{Span, Symbol};

// std crates
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;

#[derive(Debug, Clone)]
pub struct Lir<'tcx> {
    kind: LirKind<'tcx>,
    rthir: Rc<RExpr<'tcx>>,
}

impl<'tcx> Lir<'tcx> {
    fn new(kind: LirKind<'tcx>, rthir: Rc<RExpr<'tcx>>) -> Self { Self { kind, rthir } }

    pub fn get_span(&self) -> Span { self.rthir.span }

    pub fn to_smt(&self) -> String {
        use LirKind::*;

        match &self.kind {
            Declaration { name, ty } => match ty.kind() {
                TyKind::Bool => format!("(declare-const {} Bool)\n", name),
                TyKind::Int(_) => format!("(declare-const {} Int)\n", name),
                TyKind::Float(_) => format!("(declare-const {} Real)\n", name),
                _ => panic!("Unsupported variable type"),
            },
            Assert { constraint } => format!("(assert (not {}))\n", constraint),
            Assume { constraint } => format!("(assert {})\n", constraint),
        }
    }

    pub fn to_assert(&self) -> String {
        use LirKind::*;

        match &self.kind {
            Assert { constraint } => format!("(assert (not {}))\n", constraint),
            Assume { constraint } => format!("(assert (not {}))\n", constraint),
            _ => "\n".to_string(),
        }
    }

    pub fn new_parameter(name: Symbol, ty: Ty<'tcx>, pat: Rc<RExpr<'tcx>>) -> Lir<'tcx> {
        Lir::new(LirKind::Declaration { name: name.clone(), ty: ty.clone() }, pat.clone())
    }

    pub fn new_assert(constraint: String, expr: Rc<RExpr<'tcx>>) -> Lir<'tcx> {
        Lir::new(LirKind::Assert { constraint }, expr.clone())
    }

    pub fn new_assume(constraint: String, expr: Rc<RExpr<'tcx>>) -> Lir<'tcx> {
        Lir::new(LirKind::Assume { constraint }, expr.clone())
    }
}

#[derive(Debug, Clone)]
pub enum LirKind<'tcx> {
    Declaration { name: Symbol, ty: Ty<'tcx> },
    Assert { constraint: String },
    Assume { constraint: String },
}
