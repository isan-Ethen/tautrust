// rustc crates
use rustc_middle::ty::Ty;
use rustc_span::Symbol;

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

    pub fn new_parameter(name: Symbol, ty: Ty<'tcx>, pat: Rc<RExpr<'tcx>>) -> Lir<'tcx> {
        Lir::new(LirKind::Declaration { name: name.clone(), ty: ty.clone() }, pat.clone())
    }
}

#[derive(Debug, Clone)]
pub enum LirKind<'tcx> {
    Declaration { name: Symbol, ty: Ty<'tcx> },
}

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    Int(i64),
    Real(f64),
    Unit,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    DivInt,
    Mod,
    DivReal,
    And,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}
