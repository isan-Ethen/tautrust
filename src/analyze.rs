// rustc crates
use rustc_span::def_id::LocalDefId;

// std crates
use std::collections::HashMap as Map;

// Own crates
use crate::thir::rthir::*;

pub fn analyze<'tcx>(main_id: LocalDefId, fn_map: Map<LocalDefId, RThir<'tcx>>) {
  let main = fn_map.get(&main_id);
  println!("{:?}", main);
}
