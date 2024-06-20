// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
use std::io::Result;

// Own crates
use crate::print::print_thir;

pub fn drive_tautrust(tcx: TyCtxt) -> Result<()> {
  print_thir(&tcx);
  Ok(())
}
