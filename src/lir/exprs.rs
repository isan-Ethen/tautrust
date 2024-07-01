// rustc crates
use rustc_middle::thir::LocalVarId;

// std crates
use std::collections::{HashMap, VecDeque};
// Own crates
use crate::thir::*;

#[derive(Debug, Clone)]
pub struct Lir<'a, 'tcx> {
  pub body: VecDeque<LExpr>,
  pub var_dic: HashMap<&'a LocalVarId, &'a RPat<'tcx>>,
  pub lexpr_to_rexpr: HashMap<&'a LExpr, &'a RExpr<'tcx>>,
  pub nv_to_var: HashMap<&'a NumericValue, &'a RExpr<'tcx>>,
}

impl<'a, 'tcx> Lir<'a, 'tcx> {
  pub fn new() -> Self {
    Self {
      body: VecDeque::new(),
      var_dic: HashMap::new(),
      lexpr_to_rexpr: HashMap::new(),
      nv_to_var: HashMap::new(),
    }
  }

  pub fn insert_to_var_dic(&mut self, var_id: &'a LocalVarId, var: &'a RPat<'tcx>) {
    self.var_dic.insert(var_id, var);
  }
}

#[derive(Debug, Clone)]
pub enum LExpr {
  Let(NumericValue, Box<LExpr>),                  // Let expression
  Op(Operation),                                  // Operation Expression
  If(Arm),                                        // If Expression
  Variant(NumericValue),                          // Variant
  FunCall(String, Vec<LExpr>),                    // Function call
  Quant(QuantifierKind, Vec<String>, Box<LExpr>), // Quantifier
}

#[derive(Debug, Clone)]
pub enum Operation {
  BinOp(BinOpKind, Box<LExpr>, Box<LExpr>), // Binary operation
  UnOp(UnOpKind, Box<LExpr>),               // Unary operation
}

#[derive(Debug, Clone)]
pub struct Arm {
  condition: Box<LExpr>,
  then_block: VecDeque<Box<LExpr>>,
  else_block: VecDeque<Box<LExpr>>,
}

#[derive(Debug, Clone)]
pub enum NumericValue {
  Variable(String, LocalVarId),
  Const(i32),
}

#[derive(Debug, Clone)]
pub enum Assertion {
  Assert(Box<LExpr>),
  Assume(Box<LExpr>),
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
  Add,
  Sub,
  Mul,
  Div,
  Eq,
  Lt,
  Gt,
  Le,
  Ge,
  And,
  Or,
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
  Not,
}

#[derive(Debug, Clone)]
pub enum QuantifierKind {
  ForAll,
  Exists,
}
