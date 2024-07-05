// rustc crates
use rustc_span::{Span, Symbol};

// std crates
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;

pub enum Lir<'tcx> {
    Declaretion { name: String, ty: Symbol, expr: Rc<RExpr<'tcx>> },
}
