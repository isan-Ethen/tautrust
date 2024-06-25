// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates
use crate::thir::reduced_thir;
use crate::thir::RThirPrinter;

pub fn print_thir<'tcx>(tcx: &TyCtxt<'tcx>) {
  tcx.mir_keys(()).iter().for_each(|&key| {
    let rthir = reduced_thir(&tcx, key).expect("Generate RTHIR failed");
    let mut printer = RThirPrinter::new(&rthir);
    printer.print();
    println!("{}", printer.into_buffer());
  });
}
