// rustc crates
// std crates
// Own crates
use crate::thir::RThir;
mod exprs;
pub use exprs::*;
mod lir_generator;
use lir_generator::get_lir;

pub fn lir<'a, 'tcx>(rthir: &'a RThir<'tcx>) -> Lir<'a, 'tcx> { get_lir(rthir) }
