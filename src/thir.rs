// rustc crates
use rustc_errors::ErrorGuaranteed;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LocalDefId;

// std crates
// Own crates
pub mod rthir;
mod thir_printer;
mod thir_reducer;
use rthir::*;
use thir_printer::ThirPrinter;
use thir_reducer::*;

#[allow(unused)]
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

pub fn generate_rthir<'tcx>(
    tcx: &TyCtxt<'tcx>, owner_def: LocalDefId,
) -> Result<RThir<'tcx>, ErrorGuaranteed> {
    let (thir, _) = tcx.thir_body(owner_def)?;
    let thir = thir.steal();
    Ok(reduce_thir(thir))
}
