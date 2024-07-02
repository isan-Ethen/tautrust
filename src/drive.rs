// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
use std::io::Result;

// Own crates
use crate::util::enumerate_rthir;

pub fn drive_tautrust(tcx: TyCtxt) -> Result<()> {
  let rthirv = enumerate_rthir(&tcx);
  for rthir in rthirv {
    println!("{:?}", rthir);
  }
  Ok(())
}
