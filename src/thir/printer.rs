// rustc crates
use rustc_middle::thir::*;

// std crates
use std::fmt::{self, Write};

// Own crates
use crate::thir::reduced_thir::*;

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
    for param in self.rthir.params.iter() {
      self.print_param(param, 1);
    }
    print_indented!(self, "]", 0);
    "error".into()
  }

  pub fn into_buffer(self) -> String { self.fmt }

  fn print_param(&mut self, param: &RParam<'tcx>, depth_lvl: usize) {
    let RParam { pat } = param;

    print_indented!(self, "Param {", depth_lvl);

    if let Some(pat) = pat {
      print_indented!(self, "param: Some(", depth_lvl + 1);
      self.print_pat(pat, depth_lvl + 2);
      print_indented!(self, ")", depth_lvl + 1);
    } else {
      print_indented!(self, "param: None", depth_lvl + 1);
    }

    print_indented!(self, "}", depth_lvl);
  }

  fn print_pat(self, pat: &Box<RPat<'tcx>>, depth_lvl: usize) {
    let RPat { kind, span } = &**pat;

    print_indented!(self, "Pat {", depth_lvl);
    print_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    self.print_pat_kind(kind, depth_lvl + 1);
    print_indented!(self, "}", depth_lvl);
  }

  fn print_pat_kind(&mut self, pat_kind: &PatKind<'tcx>, depth_lvl: usize) {
    print_indented!(self, "kind: PatKind {", depth_lvl);

    match pat_kind {
      PatKind::Wild => {
        print_indented!(self, "Wild", depth_lvl + 1);
      }
      PatKind::Never => {
        print_indented!(self, "Never", depth_lvl + 1);
      }
      PatKind::AscribeUserType { ascription, subpattern } => {
        print_indented!(self, "AscribeUserType: {", depth_lvl + 1);
        print_indented!(self, format!("ascription: {:?}", ascription), depth_lvl + 2);
        print_indented!(self, "subpattern: ", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 3);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Binding { name, mode, var, ty, subpattern, is_primary } => {
        print_indented!(self, "Binding {", depth_lvl + 1);
        print_indented!(self, format!("name: {:?}", name), depth_lvl + 2);
        print_indented!(self, format!("mode: {:?}", mode), depth_lvl + 2);
        print_indented!(self, format!("var: {:?}", var), depth_lvl + 2);
        print_indented!(self, format!("ty: {:?}", ty), depth_lvl + 2);
        print_indented!(self, format!("is_primary: {:?}", is_primary), depth_lvl + 2);

        if let Some(subpattern) = subpattern {
          print_indented!(self, "subpattern: Some( ", depth_lvl + 2);
          self.print_pat(subpattern, depth_lvl + 3);
          print_indented!(self, ")", depth_lvl + 2);
        } else {
          print_indented!(self, "subpattern: None", depth_lvl + 2);
        }

        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Variant { adt_def, args, variant_index, subpatterns } => {
        print_indented!(self, "Variant {", depth_lvl + 1);
        print_indented!(self, "adt_def: ", depth_lvl + 2);
        self.print_adt_def(*adt_def, depth_lvl + 3);
        print_indented!(self, format!("args: {:?}", args), depth_lvl + 2);
        print_indented!(self, format!("variant_index: {:?}", variant_index), depth_lvl + 2);

        if subpatterns.len() > 0 {
          print_indented!(self, "subpatterns: [", depth_lvl + 2);
          for field_pat in subpatterns.iter() {
            self.print_pat(&field_pat.pattern, depth_lvl + 3);
          }
          print_indented!(self, "]", depth_lvl + 2);
        } else {
          print_indented!(self, "subpatterns: []", depth_lvl + 2);
        }

        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Leaf { subpatterns } => {
        print_indented!(self, "Leaf { ", depth_lvl + 1);
        print_indented!(self, "subpatterns: [", depth_lvl + 2);
        for field_pat in subpatterns.iter() {
          self.print_pat(&field_pat.pattern, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Deref { subpattern } => {
        print_indented!(self, "Deref { ", depth_lvl + 1);
        print_indented!(self, "subpattern:", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::DerefPattern { subpattern, .. } => {
        print_indented!(self, "DerefPattern { ", depth_lvl + 1);
        print_indented!(self, "subpattern:", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Constant { value } => {
        print_indented!(self, "Constant {", depth_lvl + 1);
        print_indented!(self, format!("value: {:?}", value), depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::InlineConstant { def, subpattern } => {
        print_indented!(self, "InlineConstant {", depth_lvl + 1);
        print_indented!(self, format!("def: {:?}", def), depth_lvl + 2);
        print_indented!(self, "subpattern:", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Range(pat_range) => {
        print_indented!(self, format!("Range ( {:?} )", pat_range), depth_lvl + 1);
      }
      PatKind::Slice { prefix, slice, suffix } => {
        print_indented!(self, "Slice {", depth_lvl + 1);

        print_indented!(self, "prefix: [", depth_lvl + 2);
        for prefix_pat in prefix.iter() {
          self.print_pat(prefix_pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);

        if let Some(slice) = slice {
          print_indented!(self, "slice: ", depth_lvl + 2);
          self.print_pat(slice, depth_lvl + 3);
        }

        print_indented!(self, "suffix: [", depth_lvl + 2);
        for suffix_pat in suffix.iter() {
          self.print_pat(suffix_pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);

        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Array { prefix, slice, suffix } => {
        print_indented!(self, "Array {", depth_lvl + 1);

        print_indented!(self, "prefix: [", depth_lvl + 2);
        for prefix_pat in prefix.iter() {
          self.print_pat(prefix_pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);

        if let Some(slice) = slice {
          print_indented!(self, "slice: ", depth_lvl + 2);
          self.print_pat(slice, depth_lvl + 3);
        }

        print_indented!(self, "suffix: [", depth_lvl + 2);
        for suffix_pat in suffix.iter() {
          self.print_pat(suffix_pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);

        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Or { pats } => {
        print_indented!(self, "Or {", depth_lvl + 1);
        print_indented!(self, "pats: [", depth_lvl + 2);
        for pat in pats.iter() {
          self.print_pat(pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      PatKind::Error(_) => {
        print_indented!(self, "Error", depth_lvl + 1);
      }
    }

    print_indented!(self, "}", depth_lvl);
  }
}
