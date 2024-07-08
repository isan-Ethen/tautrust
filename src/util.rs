// rustc crates
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LocalDefId;

// std crates
use std::collections::HashMap as Map;
use std::rc::Rc;

// Own crates
use crate::thir::{rthir::*, *};

pub fn get_fn_map<'tcx>(tcx: &TyCtxt<'tcx>) -> Map<LocalDefId, Rc<RThir<'tcx>>> {
    let mut map: Map<LocalDefId, Rc<RThir<'tcx>>> = Map::new();
    tcx.mir_keys(()).iter().for_each(|&key| {
        let rthir = generate_rthir(&tcx, key).expect("Generate ReducedTHIR failed");
        println!("{:?}, {:?}", key, rthir);
        map.insert(key, Rc::new(rthir));
    });
    map
}
