// rustc crates
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LocalDefId;

// std crates
// Own crates
mod printer;

pub fn thir_tree(tcx: &TyCtxt<'_>, owner_def: LocalDefId) -> String {
  match tcx.thir_body(owner_def) {
    Ok((thir, _)) => {
      let thir = thir.steal();
      let mut printer = printer::ThirPrinter::new(&thir);
      printer.print();
      printer.into_buffer()
    }
    Err(_) => "error".into(),
  }
}
