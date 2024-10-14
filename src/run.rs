// rustc_crates
use rustc_driver::{Callbacks, Compilation, RunCompiler};
use rustc_interface::{
    interface::{
        Compiler, // Config
    },
    Queries,
};

// std crates
use std::env::args as get_args;
use std::sync::OnceLock;

// Own crates
use crate::drive::drive_tautrust;

pub static FILE: OnceLock<String> = OnceLock::new();

struct MyCallbacks {}

impl Callbacks for MyCallbacks {
    fn after_crate_root_parsing<'tcx>(
        &mut self, _compiler: &Compiler, queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            drive_tautrust(tcx);
        });
        Compilation::Stop
    }
}

pub fn run_tautrust() {
    println!("Tautrust!\n");
    let mut args = Vec::new();
    let mut args_iter = get_args();
    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            _ => args.push(arg),
        };
    }
    FILE.set(args[1].to_string()).unwrap();
    RunCompiler::new(&args, &mut MyCallbacks {}).run().unwrap();
}
