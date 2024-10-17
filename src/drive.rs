// rustc crates
use rustc_middle::ty::TyCtxt;
use rustc_span::{Span, SpanData};

// std crates
use std::fs;
use std::path::Path;

// Own crates
use crate::analyze::{analyze, AnalysisError};
use crate::run::FILE;
use crate::util::get_fn_map;
use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

pub fn drive_tautrust(tcx: TyCtxt) {
    if let Some((main_id, ..)) = tcx.entry_fn(()) {
        let fn_map = get_fn_map(&tcx);
        if let Err(error) = analyze(main_id.expect_local(), fn_map, tcx) {
            use AnalysisError::*;
            match error {
                FunctionNotFound(id) => eprintln!("Function not found: {:?}", id),
                UnsupportedPattern(pattern) => eprintln!("Unsupported pattern: {}", pattern),
                VerifyError { span } => {
                    print_error(span);
                } // _ => unreachable!(),
            }
        } else {
            println!("\x1b[92mAll verification success!\x1b[0m\n");
        }
    } else {
        eprintln!("No main function!")
    }
}

fn print_error(span: Span) {
    let mut colors = ColorGenerator::new();

    let file = FILE.get().expect("No file given");
    let file_content = fs::read_to_string(Path::new(file)).expect("Read file content failed");
    let SpanData { lo, hi, .. } = span.data();
    Report::build(ReportKind::Error, format!("{:?}", span).as_str(), 0)
        .with_code(1)
        .with_message(format!("Verification Error"))
        .with_label(
            Label::new((file.as_str(), lo.0 as usize..hi.0 as usize))
                .with_message(format!("The condition is not satisfied"))
                .with_color(colors.next()),
        )
        .finish()
        .print((file.as_str(), Source::from(file_content)))
        .unwrap();
}
