// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
use std::io::Result;

// Own crates
use crate::analyze::analyze;
use crate::util::get_fn_map;

pub fn drive_tautrust(tcx: TyCtxt) -> Result<()> {
  if let Some((main_id, ..)) = tcx.entry_fn(()) {
    let fn_map = get_fn_map(&tcx);
    analyze(main_id.expect_local(), fn_map);
  } else {
    panic!("No main function!")
  }
  Ok(())
}
