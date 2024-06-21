// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates
use crate::thir::thir_tree;

pub fn print_thir<'tcx>(tcx: &TyCtxt<'tcx>) {
  tcx.mir_keys(()).iter().for_each(|&key| {
    println!("{}", thir_tree(&tcx, key));
    // println!("{:?}", tcx.thir_body(key).unwrap().0.steal());
  });
}
