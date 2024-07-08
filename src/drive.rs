// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates
use crate::analyze::analyze;
use crate::util::get_fn_map;

pub fn drive_tautrust(tcx: TyCtxt) {
    if let Some((main_id, ..)) = tcx.entry_fn(()) {
        let fn_map = get_fn_map(&tcx);
        if let Err(why) = analyze(main_id.expect_local(), fn_map) {
            eprintln!("{:?}", why);
        } else {
            println!("Verification success!")
        }
    } else {
        panic!("No main function!")
    }
}
