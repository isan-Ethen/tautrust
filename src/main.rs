#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(pattern)]

// Extern rustc crates
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

#[macro_use]
mod util;
mod analyze;
mod drive;
mod lir;
mod run;
mod thir;

fn main() { run::run_tautrust(); }
