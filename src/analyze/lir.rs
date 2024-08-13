// rustc crates
use rustc_middle::ty::Ty;
use rustc_span::Span;

// std crates
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;

#[derive(Debug, Clone)]
pub struct Lir<'tcx> {
    pub name: String,
    pub ty: Ty<'tcx>,
    pub assume: String,
    pub expr: Rc<RExpr<'tcx>>,
}

impl<'tcx> Lir<'tcx> {
    pub fn new(name: String, ty: Ty<'tcx>, assume: String, expr: Rc<RExpr<'tcx>>) -> Self {
        Self { name, ty, assume, expr }
    }

    pub fn get_span(&self) -> Span { self.expr.span }

    pub fn to_smt(&self) -> String { self.assume.clone() }

    pub fn set_assume(&mut self, constraint: String) { self.assume = constraint }

    pub fn adapt_assume(&mut self, operation: String, arg: String, expr: Rc<RExpr<'tcx>>) {
        self.assume = format!("({} {} {})", operation, self.assume, arg);
        self.expr = expr;
    }
}
