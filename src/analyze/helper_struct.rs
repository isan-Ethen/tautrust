// rustc crates
use rustc_span::{def_id::LocalDefId, Span};

// std crates
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;

#[derive(Debug)]
pub enum AnalysisType<'tcx> {
    Return(Option<String>),
    Invariant(Rc<RExpr<'tcx>>),
    Break,
    Other,
}

#[derive(Debug)]
pub enum AnalysisError {
    FunctionNotFound(LocalDefId),
    UnsupportedPattern(String),
    RandFunctions,
    VerifyError { span: Span },
}
