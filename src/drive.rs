// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
use std::io::Result;

// Own crates
use crate::util::get_fn_map;

pub fn drive_tautrust(tcx: TyCtxt) -> Result<()> {
  if let Some((main_id, ..)) = tcx.entry_fn(()) {
    let rthirv = get_fn_map(&tcx);
    for rthir in rthirv {
      println!("{:?}", rthir);
    }
  } else {
    panic!("No main function!")
  }
  Ok(())
}
