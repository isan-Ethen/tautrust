// rustc crates
// std crates
// Own crates
use crate::lir::*;
use crate::thir::*;

pub fn get_lir<'a, 'tcx>(thir: &'a RThir<'tcx>) -> Lir<'a, 'tcx> {
  LirGenerator::new(thir).generate_lir()
}

#[derive(Debug)]
struct LirGenerator<'a, 'tcx> {
  rthir: &'a RThir<'tcx>,
  lir: Lir<'a, 'tcx>,
}

impl<'a, 'tcx> LirGenerator<'a, 'tcx> {
  fn new(rthir: &'a RThir<'tcx>) -> Self { Self { rthir, lir: Lir::new() } }

  fn generate_lir(mut self) -> Lir<'a, 'tcx> {
    self.handle_param(&self.rthir.params);
    self.lir
  }

  fn handle_param(&mut self, params: &'a Vec<RParam<'tcx>>) {
    for param in params.iter() {
      self.param_to_variant(param);
    }
  }

  fn param_to_variant(&mut self, param: &'a RParam<'tcx>) {
    let RParam { pat } = param;
    if let Some(pat) = pat {
      let nv = self.pat_to_variant(pat);
      self.lir.body.push_back(LExpr::Variant(nv));
    } else {
      panic!("Expected a pattern in RParam, but found None");
    }
  }

  fn pat_to_variant(&mut self, pat: &'a RPat<'tcx>) -> NumericValue {
    match &pat.kind {
      RPatKind::Binding { name, var, .. } => {
        self.lir.var_dic.entry(&var).or_insert(&pat);
        NumericValue::Variable(name.to_string(), var.clone())
      }
      _ => panic!("Unknown param pattern: {:?}", pat.kind),
    }
  }
}
