// rustc crates
use rustc_middle::ty::TyCtxt;

// std crates
// Own crates
use crate::analyze::{analyze, AnalysisError};
use crate::util::get_fn_map;

pub fn drive_tautrust(tcx: TyCtxt) {
    if let Some((main_id, ..)) = tcx.entry_fn(()) {
        let fn_map = get_fn_map(&tcx);
        if let Err(error) = analyze(main_id.expect_local(), fn_map) {
            use AnalysisError::*;
            match error {
                FunctionNotFound(id) => eprintln!("Function not found: {:?}", id),
                UnsupportedPattern(pattern) => eprintln!("Unsupported pattern: {}", pattern),
                VerifyError { message, span } => eprintln!("{}: {:?}", message, span),
                _ => unreachable!(),
            }
        } else {
            println!("All verification success!")
        }
    } else {
        panic!("No main function!")
    }
}
