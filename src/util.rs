// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates
use crate::thir::*;

pub fn enumerate_rthir<'tcx>(tcx: &TyCtxt<'tcx>) -> Vec<RThir<'tcx>> {
  let mut vec = Vec::new();
  tcx.mir_keys(()).iter().for_each(|&key| {
    let rthir = reduced_thir(&tcx, key).expect("Generate ReducedTHIR failed");
    vec.push(rthir);
  });
  vec
}
