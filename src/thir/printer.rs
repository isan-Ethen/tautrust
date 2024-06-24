// rustc crates
// std crates
use std::fmt::{self, Write};

// Own crates
use crate::thir::reduced_thir::RThir;

pub struct RThirPrinter<'a, 'tcx> {
  rthir: &'a RThir<'tcx>,
  fmt: String,
}

const INDENT: &str = "  ";

macro_rules! print_indented {
  ($writer:ident, $s:expr, $indent_lvl:expr) => {
    $writer.indent($indent_lvl);
    writeln!($writer, "{}", $s).expect("unable to write to ThirPrinter");
  };
}

impl<'a, 'tcx> Write for RThirPrinter<'a, 'tcx> {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    self.fmt.push_str(s);
    Ok(())
  }
}

impl<'a, 'tcx> RThirPrinter<'a, 'tcx> {
  pub fn new(rthir: &'a RThir<'tcx>) -> Self { Self { rthir, fmt: String::new() } }

  fn indent(&mut self, level: usize) {
    for _ in 0..level {
      self.fmt.push_str(INDENT);
    }
  }

  fn print(&mut self) -> String {
    print_indented!(self, "params: [", 0);
    // for param in self.rthir.params.iter() {
    //   self.print_param(param, 1);
    // }
    print_indented!(self, "]", 0);
    "error".into()
  }

  pub fn into_buffer(self) -> String { self.fmt }

  // fn prnt_param(&mut self, param: &Param)
}
