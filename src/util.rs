// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates

pub fn print_thir<'tcx>(tcx: TyCtxt<'tcx>) {
  tcx.mir_keys(()).iter().for_each(|&key| {
    // thir_body needs WithOptConstParam<LocalDfId>
    let thir = tcx.thir_body(key).expect("Typeck failed!");
    // println!("thir_body: {:?}", thir)
    let stolen_thir = thir.0.steal();
    println!("stolen_thir: {:?}", stolen_thir);
  });
}
