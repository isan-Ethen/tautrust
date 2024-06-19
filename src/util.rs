// rustc crates
use rustc_middle::ty::{TyCtxt, WithOptConstParam};

// std crates
// Own crates

pub fn print_thir<'tcx>(tcx: TyCtxt<'tcx>) {
  tcx.mir_keys(()).iter().for_each(|&key| {
    // thir_body needs WithOptConstParam<LocalDfId>
    let thir = tcx.thir_body(WithOptConstParam::unknown(key));
    println!("thir_body: {:?}", thir)
  });
}
