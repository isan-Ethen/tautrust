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
    pub assume: (String, String),
    pub mutable_ref: bool,
    pub expr: Rc<RExpr<'tcx>>,
}

impl<'tcx> Lir<'tcx> {
    pub fn new(name: String, ty: Ty<'tcx>, assume: String, expr: Rc<RExpr<'tcx>>) -> Self {
        Self { name, ty, assume: (assume, String::new()), mutable_ref: false, expr }
    }

    pub fn new_mut_ref(name: String, ty: Ty<'tcx>, assume: String, expr: Rc<RExpr<'tcx>>) -> Self {
        Self { name, ty, assume: (assume, String::new()), mutable_ref: true, expr }
    }

    pub fn get_span(&self) -> Span { self.expr.span }

    pub fn to_smt(&self) -> String { self.assume.0.clone() }

    pub fn set_assume(&mut self, constraint: String) { self.assume.0 = constraint }

    pub fn set_prophecy(&mut self, constraint: String) { self.assume.1 = constraint }

    pub fn adapt_assume(&mut self, operation: String, arg: String, expr: Rc<RExpr<'tcx>>) {
        self.assume.0 = format!("({} {} {})", operation, self.assume.0, arg);
        println!("{}", self.assume.0);
        self.expr = expr;
    }
}
