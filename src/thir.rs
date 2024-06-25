// rustc crates
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LocalDefId;

// std crates
// Own crates
mod rthir;
mod thir_printer;
mod thir_reducer;
pub use rthir::*;
use thir_printer::ThirPrinter;
use thir_reducer::ThirReducer;

pub fn thir_tree(tcx: &TyCtxt<'_>, owner_def: LocalDefId) -> String {
  match tcx.thir_body(owner_def) {
    Ok((thir, _)) => {
      let thir = thir.steal();
      let mut printer = ThirPrinter::new(&thir);
      printer.print();
      printer.into_buffer()
    }
    Err(_) => "error".into(),
  }
}

pub fn reduced_thir<'tcx>(
  tcx: &TyCtxt<'tcx>, owner_def: LocalDefId,
) -> Result<RThir<'tcx>, String> {
  match tcx.thir_body(owner_def) {
    Ok((thir, _)) => {
      let thir = thir.steal();
      let mut reducer = ThirReducer::new(&thir);
      reducer.reduce();
      let reduced_thir = reducer.get_reduced_thir();

      Ok(reduced_thir)
    }
    Err(_) => Err("error".into()),
  }
}
