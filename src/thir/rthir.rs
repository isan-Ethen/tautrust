// rustc crates
use rustc_errors::ErrorGuaranteed;
use rustc_hir as hir;
use rustc_hir::def_id::DefId;
use rustc_hir::BindingMode;
use rustc_middle::middle::region;
use rustc_middle::mir;
use rustc_middle::mir::{BinOp, BorrowKind, UnOp};
use rustc_middle::thir::*;
use rustc_middle::ty::adjustment::PointerCoercion;
use rustc_middle::ty::{self, CanonicalUserType, GenericArgsRef, Ty};
use rustc_span::{Span, Symbol};
use rustc_target::abi::{FieldIdx, VariantIdx};

// std crates
use std::fmt;
// Own crates

// R: Reduced
pub struct RThir<'tcx> {
  pub params: Vec<RParam<'tcx>>,
  pub body: Option<RExpr<'tcx>>,
}

impl<'tcx> RThir<'tcx> {
  pub fn new() -> Self { Self { params: Vec::new(), body: None } }

  pub fn set_params(&mut self, new_params: Vec<RParam<'tcx>>) { self.params = new_params.clone(); }

  pub fn set_body(&mut self, new_body: Option<RExpr<'tcx>>) { self.body = new_body; }
}

impl<'tcx> fmt::Debug for RThir<'tcx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", RThirFormatter::get_formated_rthir(self))
  }
}

struct RThirFormatter<'a, 'tcx> {
  rthir: &'a RThir<'tcx>,
  fmt: String,
}

const INDENT: &str = "  ";

impl<'a, 'tcx> RThirFormatter<'a, 'tcx> {
  fn new(rthir: &'a RThir<'tcx>) -> Self { Self { rthir, fmt: String::new() } }

  fn get_formated_rthir(rthir: &'a RThir<'tcx>) -> String {
    let mut formatter = RThirFormatter::new(rthir);
    formatter.format();
    formatter.fmt
  }

  fn add_indented_string(&mut self, string: &str, indent_lvl: usize) {
    self.indent(indent_lvl);
    self.fmt.push_str(string);
    self.fmt.push_str("\n");
  }

  fn indent(&mut self, level: usize) {
    for _ in 0..level {
      self.fmt.push_str(INDENT);
    }
  }

  pub fn format(&mut self) {
    self.add_indented_string("params: [", 0);

    for param in self.rthir.params.iter() {
      self.format_param(param, 1);
    }

    self.add_indented_string("]", 0);

    self.add_indented_string("body:", 0);
    if let Some(body) = &self.rthir.body {
      self.format_expr(&body, 1);
    } else {
      self.add_indented_string("None", 1);
    }
  }

  fn format_param(&mut self, param: &RParam<'tcx>, depth_lvl: usize) {
    let RParam { pat } = param;

    self.add_indented_string("Param {", depth_lvl);

    if let Some(pat) = pat {
      self.add_indented_string("param: Some(", depth_lvl + 1);
      self.format_pat(pat, depth_lvl + 2);
      self.add_indented_string(")", depth_lvl + 1);
    } else {
      self.add_indented_string("param: None", depth_lvl + 1);
    }

    self.add_indented_string("}", depth_lvl);
  }

  fn format_pat(&mut self, pat: &RPat<'tcx>, depth_lvl: usize) {
    let RPat { kind, span } = pat;

    self.add_indented_string("Pat {", depth_lvl);
    self.add_indented_string(&format!("span: {:?}", span), depth_lvl + 1);
    self.format_pat_kind(kind, depth_lvl + 1);
    self.add_indented_string("}", depth_lvl);
  }

  fn format_pat_kind(&mut self, pat_kind: &RPatKind<'tcx>, depth_lvl: usize) {
    self.add_indented_string("kind: PatKind {", depth_lvl);

    match pat_kind {
      RPatKind::Never => {
        self.add_indented_string("Never", depth_lvl + 1);
      }
      RPatKind::AscribeUserType { ascription, subpattern } => {
        self.add_indented_string("AscribeUserType: {", depth_lvl + 1);
        self.add_indented_string(&format!("ascription: {:?}", ascription), depth_lvl + 2);
        self.add_indented_string("subpattern: ", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 3);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::Binding { name, mode, var, ty, subpattern, is_primary } => {
        self.add_indented_string("Binding {", depth_lvl + 1);
        self.add_indented_string(&format!("name: {:?}", name), depth_lvl + 2);
        self.add_indented_string(&format!("mode: {:?}", mode), depth_lvl + 2);
        self.add_indented_string(&format!("var: {:?}", var), depth_lvl + 2);
        self.add_indented_string(&format!("ty: {:?}", ty), depth_lvl + 2);
        self.add_indented_string(&format!("is_primary: {:?}", is_primary), depth_lvl + 2);

        if let Some(subpattern) = subpattern {
          self.add_indented_string("subpattern: Some( ", depth_lvl + 2);
          self.format_pat(subpattern, depth_lvl + 3);
          self.add_indented_string(")", depth_lvl + 2);
        } else {
          self.add_indented_string("subpattern: None", depth_lvl + 2);
        }

        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::Deref { subpattern } => {
        self.add_indented_string("Deref { ", depth_lvl + 1);
        self.add_indented_string("subpattern:", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::DerefPattern { subpattern, mutability } => {
        self.add_indented_string("DerefPattern { ", depth_lvl + 1);
        self.add_indented_string(&format!("mutability: {:?}", mutability), depth_lvl + 2);
        self.add_indented_string("subpattern:", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::Constant { value } => {
        self.add_indented_string("Constant {", depth_lvl + 1);
        self.add_indented_string(&format!("value: {:?}", value), depth_lvl + 2);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::Range(pat_range) => {
        self.add_indented_string(&format!("Range ( {:?} )", pat_range), depth_lvl + 1);
      }
      RPatKind::Or { pats } => {
        self.add_indented_string("Or {", depth_lvl + 1);
        self.add_indented_string("pats: [", depth_lvl + 2);
        for pat in pats.iter() {
          self.format_pat(pat, depth_lvl + 3);
        }
        self.add_indented_string("]", depth_lvl + 2);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RPatKind::Error(_) => {
        self.add_indented_string("Error", depth_lvl + 1);
      }
    }

    self.add_indented_string("}", depth_lvl);
  }

  fn format_expr(&mut self, expr: &RExpr<'tcx>, depth_lvl: usize) {
    let RExpr { span, kind } = expr;
    self.add_indented_string("Expr {", depth_lvl);
    self.add_indented_string(&format!("span: {:?}", span), depth_lvl + 1);
    self.add_indented_string("kind: ", depth_lvl + 1);
    self.format_expr_kind(kind, depth_lvl + 2);
    self.add_indented_string("}", depth_lvl);
  }

  fn format_expr_kind(&mut self, expr_kind: &RExprKind<'tcx>, depth_lvl: usize) {
    use RExprKind::*;

    match expr_kind {
      Box { value } => {
        self.add_indented_string("Box {", depth_lvl);
        self.format_expr(value, depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      If { cond, then, else_opt } => {
        self.add_indented_string("If {", depth_lvl);
        self.add_indented_string("cond:", depth_lvl + 1);
        self.format_expr(cond, depth_lvl + 2);
        self.add_indented_string("then:", depth_lvl + 1);
        self.format_expr(then, depth_lvl + 2);

        if let Some(else_expr) = else_opt {
          self.add_indented_string("else:", depth_lvl + 1);
          self.format_expr(else_expr, depth_lvl + 2);
        }

        self.add_indented_string("}", depth_lvl);
      }
      Call { fun, args, ty, from_hir_call, fn_span } => {
        self.add_indented_string("Call {", depth_lvl);
        self.add_indented_string(&format!("ty: {:?}", ty), depth_lvl + 1);
        self.add_indented_string(&format!("from_hir_call: {}", from_hir_call), depth_lvl + 1);
        self.add_indented_string(&format!("fn_span: {:?}", fn_span), depth_lvl + 1);
        self.add_indented_string("fun:", depth_lvl + 1);
        self.format_expr(fun, depth_lvl + 2);

        if args.len() > 0 {
          self.add_indented_string("args: [", depth_lvl + 1);
          for arg in args.iter() {
            self.format_expr(arg, depth_lvl + 2);
          }
          self.add_indented_string("]", depth_lvl + 1);
        } else {
          self.add_indented_string("args: []", depth_lvl + 1);
        }

        self.add_indented_string("}", depth_lvl);
      }
      Deref { arg } => {
        self.add_indented_string("Deref {", depth_lvl);
        self.format_expr(arg, depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Binary { op, lhs, rhs } => {
        self.add_indented_string("Binary {", depth_lvl);
        self.add_indented_string(&format!("op: {:?}", op), depth_lvl + 1);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      LogicalOp { op, lhs, rhs } => {
        self.add_indented_string("LogicalOp {", depth_lvl);
        self.add_indented_string(&format!("op: {:?}", op), depth_lvl + 1);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Unary { op, arg } => {
        self.add_indented_string("Unary {", depth_lvl);
        self.add_indented_string(&format!("op: {:?}", op), depth_lvl + 1);
        self.add_indented_string("arg:", depth_lvl + 1);
        self.format_expr(arg, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Cast { source } => {
        self.add_indented_string("Cast {", depth_lvl);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Use { source } => {
        self.add_indented_string("Use {", depth_lvl);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      NeverToAny { source } => {
        self.add_indented_string("NeverToAny {", depth_lvl);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      PointerCoercion { cast, source } => {
        self.add_indented_string("Pointer {", depth_lvl);
        self.add_indented_string(&format!("cast: {:?}", cast), depth_lvl + 1);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Loop { body } => {
        self.add_indented_string("Loop (", depth_lvl);
        self.add_indented_string("body:", depth_lvl + 1);
        self.format_expr(body, depth_lvl + 2);
        self.add_indented_string(")", depth_lvl);
      }
      Let { expr, pat } => {
        self.add_indented_string("Let {", depth_lvl);
        self.add_indented_string("expr:", depth_lvl + 1);
        self.format_expr(expr, depth_lvl + 2);
        self.add_indented_string(&format!("pat: {:?}", pat), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Match { scrutinee, arms, .. } => {
        self.add_indented_string("Match {", depth_lvl);
        self.add_indented_string("scrutinee:", depth_lvl + 1);
        self.format_expr(scrutinee, depth_lvl + 2);

        self.add_indented_string("arms: [", depth_lvl + 1);
        for arm_id in arms.iter() {
          self.format_arm(arm_id, depth_lvl + 2);
        }
        self.add_indented_string("]", depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Block { block } => self.format_block(block, depth_lvl),
      Assign { lhs, rhs } => {
        self.add_indented_string("Assign {", depth_lvl);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      AssignOp { op, lhs, rhs } => {
        self.add_indented_string("AssignOp {", depth_lvl);
        self.add_indented_string(&format!("op: {:?}", op), depth_lvl + 1);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Field { lhs, variant_index, name } => {
        self.add_indented_string("Field {", depth_lvl);
        self.add_indented_string(&format!("variant_index: {:?}", variant_index), depth_lvl + 1);
        self.add_indented_string(&format!("name: {:?}", name), depth_lvl + 1);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Index { lhs, index } => {
        self.add_indented_string("Index {", depth_lvl);
        self.add_indented_string(&format!("index: {:?}", index), depth_lvl + 1);
        self.add_indented_string("lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      VarRef { id } => {
        self.add_indented_string("VarRef {", depth_lvl);
        self.add_indented_string(&format!("id: {:?}", id), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      UpvarRef { closure_def_id, var_hir_id } => {
        self.add_indented_string("UpvarRef {", depth_lvl);
        self.add_indented_string(&format!("closure_def_id: {:?}", closure_def_id), depth_lvl + 1);
        self.add_indented_string(&format!("var_hir_id: {:?}", var_hir_id), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Borrow { borrow_kind, arg } => {
        self.add_indented_string("Borrow (", depth_lvl);
        self.add_indented_string(&format!("borrow_kind: {:?}", borrow_kind), depth_lvl + 1);
        self.add_indented_string("arg:", depth_lvl + 1);
        self.format_expr(arg, depth_lvl + 2);
        self.add_indented_string(")", depth_lvl);
      }
      Break { label, value } => {
        self.add_indented_string("Break (", depth_lvl);
        self.add_indented_string(&format!("label: {:?}", label), depth_lvl + 1);

        if let Some(value) = value {
          self.add_indented_string("value:", depth_lvl + 1);
          self.format_expr(value, depth_lvl + 2);
        }

        self.add_indented_string(")", depth_lvl);
      }
      Continue { label } => {
        self.add_indented_string("Continue {", depth_lvl);
        self.add_indented_string(&format!("label: {:?}", label), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Return { value } => {
        self.add_indented_string("Return {", depth_lvl);
        self.add_indented_string("value:", depth_lvl + 1);

        if let Some(value) = value {
          self.format_expr(value, depth_lvl + 2);
        }

        self.add_indented_string("}", depth_lvl);
      }
      Repeat { value, count } => {
        self.add_indented_string("Repeat {", depth_lvl);
        self.add_indented_string(&format!("count: {:?}", count), depth_lvl + 1);
        self.add_indented_string("value:", depth_lvl + 1);
        self.format_expr(value, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Array { fields } => {
        self.add_indented_string("Array {", depth_lvl);
        self.add_indented_string("fields: [", depth_lvl + 1);
        for field in fields.iter() {
          self.format_expr(field, depth_lvl + 2);
        }
        self.add_indented_string("]", depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      Tuple { fields } => {
        self.add_indented_string("Tuple {", depth_lvl);
        self.add_indented_string("fields: [", depth_lvl + 1);
        for field_id in fields.iter() {
          self.format_expr(field_id, depth_lvl + 2);
        }
        self.add_indented_string("]", depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      PlaceTypeAscription { source, user_ty } => {
        self.add_indented_string("PlaceTypeAscription {", depth_lvl);
        self.add_indented_string(&format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      ValueTypeAscription { source, user_ty } => {
        self.add_indented_string("ValueTypeAscription {", depth_lvl);
        self.add_indented_string(&format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        self.add_indented_string("source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
      }
      Literal { lit, neg } => {
        self.add_indented_string(&format!("Literal( lit: {:?}, neg: {:?})\n", lit, neg), depth_lvl);
      }
      NonHirLiteral { lit, user_ty } => {
        self.add_indented_string("NonHirLiteral {", depth_lvl);
        self.add_indented_string(&format!("lit: {:?}", lit), depth_lvl + 1);
        self.add_indented_string(&format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      ZstLiteral { user_ty } => {
        self.add_indented_string(&format!("ZstLiteral(user_ty: {:?})", user_ty), depth_lvl);
      }
      NamedConst { def_id, args, user_ty } => {
        self.add_indented_string("NamedConst {", depth_lvl);
        self.add_indented_string(&format!("def_id: {:?}", def_id), depth_lvl + 1);
        self.add_indented_string(&format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        self.add_indented_string(&format!("args: {:?}", args), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
      ConstParam { param, def_id } => {
        self.add_indented_string("ConstParam {", depth_lvl);
        self.add_indented_string(&format!("def_id: {:?}", def_id), depth_lvl + 1);
        self.add_indented_string(&format!("param: {:?}", param), depth_lvl + 1);
        self.add_indented_string("}", depth_lvl);
      }
    }
  }

  fn format_arm(&mut self, arm: &RArm<'tcx>, depth_lvl: usize) {
    let RArm { pattern, guard, body, span } = arm;

    self.add_indented_string("pattern: ", depth_lvl + 1);
    self.format_pat(pattern, depth_lvl + 2);

    if let Some(guard) = guard {
      self.add_indented_string("guard: ", depth_lvl + 1);
      self.format_expr(guard, depth_lvl + 2);
    } else {
      self.add_indented_string("guard: None", depth_lvl + 1);
    }

    self.add_indented_string("body: ", depth_lvl + 1);
    self.format_expr(body, depth_lvl + 2);
    self.add_indented_string(&format!("span: {:?}", span), depth_lvl + 1);
    self.add_indented_string("}", depth_lvl);
  }

  fn format_block(&mut self, block: &RBlock<'tcx>, depth_lvl: usize) {
    let RBlock { expr, stmts } = block;

    self.add_indented_string("Block {", depth_lvl);

    if stmts.len() > 0 {
      self.add_indented_string("stmts: [", depth_lvl + 1);
      for stmt in stmts.iter() {
        self.format_stmt(stmt, depth_lvl + 2);
      }
      self.add_indented_string("]", depth_lvl + 1);
    } else {
      self.add_indented_string("stmts: []", depth_lvl + 1);
    }

    if let Some(expr) = expr {
      self.add_indented_string("expr:", depth_lvl + 1);
      self.format_expr(expr, depth_lvl + 2);
    } else {
      self.add_indented_string("expr: []", depth_lvl + 1);
    }

    self.add_indented_string("}", depth_lvl);
  }

  fn format_stmt(&mut self, stmt: &RStmt<'tcx>, depth_lvl: usize) {
    let RStmt { kind } = stmt;

    self.add_indented_string("Stmt {", depth_lvl);

    match kind {
      RStmtKind::Expr { expr } => {
        self.add_indented_string("kind: Expr {", depth_lvl + 1);
        self.add_indented_string("expr:", depth_lvl + 2);
        self.format_expr(expr, depth_lvl + 3);
        self.add_indented_string("}", depth_lvl + 1);
      }
      RStmtKind::Let { pattern, initializer, else_block, span } => {
        self.add_indented_string("kind: Let {", depth_lvl + 1);

        self.add_indented_string("pattern: ", depth_lvl + 2);
        self.format_pat(pattern, depth_lvl + 3);
        self.add_indented_string(",", depth_lvl + 2);

        if let Some(init) = initializer {
          self.add_indented_string("initializer: Some(", depth_lvl + 2);
          self.format_expr(init, depth_lvl + 3);
          self.add_indented_string(")", depth_lvl + 2);
        } else {
          self.add_indented_string("initializer: None", depth_lvl + 2);
        }

        if let Some(else_block) = else_block {
          self.add_indented_string("else_block: Some(", depth_lvl + 2);
          self.format_block(else_block, depth_lvl + 3);
          self.add_indented_string(")", depth_lvl + 2);
        } else {
          self.add_indented_string("else_block: None", depth_lvl + 2);
        }

        self.add_indented_string(&format!("span: {:?}", span), depth_lvl + 2);
        self.add_indented_string("}", depth_lvl + 1);
      }
    }

    self.add_indented_string("}", depth_lvl);
  }
}

#[derive(Clone, Debug)]
pub struct RParam<'tcx> {
  pub pat: Option<Box<RPat<'tcx>>>,
}

#[derive(Clone, Debug)]
pub struct RPat<'tcx> {
  pub kind: RPatKind<'tcx>,
  pub span: Span,
}

impl<'tcx> RPat<'tcx> {
  pub fn new(kind: RPatKind<'tcx>, span: Span) -> Self { Self { kind, span } }
}

#[derive(Clone, Debug)]
pub enum RPatKind<'tcx> {
  AscribeUserType {
    ascription: Ascription<'tcx>,
    subpattern: Box<RPat<'tcx>>,
  },

  Binding {
    name: Symbol,
    mode: BindingMode,
    var: LocalVarId,
    ty: Ty<'tcx>,
    subpattern: Option<Box<RPat<'tcx>>>,
    is_primary: bool,
  },

  Deref {
    subpattern: Box<RPat<'tcx>>,
  },

  DerefPattern {
    subpattern: Box<RPat<'tcx>>,
    mutability: hir::Mutability,
  },

  Constant {
    value: mir::Const<'tcx>,
  },

  Range(Box<PatRange<'tcx>>),

  Or {
    pats: Box<[Box<RPat<'tcx>>]>,
  },

  Never,

  Error(ErrorGuaranteed),
}

#[derive(Clone, Debug)]
pub struct RExpr<'tcx> {
  pub kind: Box<RExprKind<'tcx>>,
  pub span: Span,
}

impl<'tcx> RExpr<'tcx> {
  pub fn new(kind: RExprKind<'tcx>, span: Span) -> Self { Self { kind: Box::new(kind), span } }
}

type UserTy<'tcx> = Option<Box<CanonicalUserType<'tcx>>>;

#[derive(Clone, Debug)]
pub enum RExprKind<'tcx> {
  Box {
    value: RExpr<'tcx>,
  },
  If {
    cond: RExpr<'tcx>,
    then: RExpr<'tcx>,
    else_opt: Option<RExpr<'tcx>>,
  },
  Call {
    ty: Ty<'tcx>,
    fun: RExpr<'tcx>,
    args: Box<[RExpr<'tcx>]>,
    from_hir_call: bool,
    fn_span: Span,
  },
  Deref {
    arg: RExpr<'tcx>,
  },
  Binary {
    op: BinOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  LogicalOp {
    op: LogicalOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  Unary {
    op: UnOp,
    arg: RExpr<'tcx>,
  },
  Cast {
    source: RExpr<'tcx>,
  },
  Use {
    source: RExpr<'tcx>,
  },
  NeverToAny {
    source: RExpr<'tcx>,
  },
  PointerCoercion {
    cast: PointerCoercion,
    source: RExpr<'tcx>,
  },
  Loop {
    body: RExpr<'tcx>,
  },
  Let {
    expr: RExpr<'tcx>,
    pat: Box<Pat<'tcx>>,
  },
  Match {
    scrutinee: RExpr<'tcx>,
    arms: Vec<RArm<'tcx>>,
  },
  Block {
    block: RBlock<'tcx>,
  },
  Assign {
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  AssignOp {
    op: BinOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  Field {
    lhs: RExpr<'tcx>,
    variant_index: VariantIdx,
    name: FieldIdx,
  },
  Index {
    lhs: RExpr<'tcx>,
    index: RExpr<'tcx>,
  },
  VarRef {
    id: LocalVarId,
  },
  UpvarRef {
    closure_def_id: DefId,
    var_hir_id: LocalVarId,
  },
  Borrow {
    borrow_kind: BorrowKind,
    arg: RExpr<'tcx>,
  },
  Break {
    label: region::Scope,
    value: Option<RExpr<'tcx>>,
  },
  Continue {
    label: region::Scope,
  },
  Return {
    value: Option<RExpr<'tcx>>,
  },
  Repeat {
    value: RExpr<'tcx>,
    count: ty::Const<'tcx>,
  },
  Array {
    fields: Box<[RExpr<'tcx>]>,
  },
  Tuple {
    fields: Box<[RExpr<'tcx>]>,
  },
  PlaceTypeAscription {
    source: RExpr<'tcx>,
    user_ty: UserTy<'tcx>,
  },
  ValueTypeAscription {
    source: RExpr<'tcx>,
    user_ty: UserTy<'tcx>,
  },
  Literal {
    lit: &'tcx hir::Lit,
    neg: bool,
  },
  NonHirLiteral {
    lit: ty::ScalarInt,
    user_ty: UserTy<'tcx>,
  },
  ZstLiteral {
    user_ty: UserTy<'tcx>,
  },
  NamedConst {
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
    user_ty: UserTy<'tcx>,
  },
  ConstParam {
    param: ty::ParamConst,
    def_id: DefId,
  },
}

#[derive(Clone, Debug)]
pub struct RArm<'tcx> {
  pub pattern: RPat<'tcx>,
  pub guard: Option<RExpr<'tcx>>,
  pub body: RExpr<'tcx>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct RBlock<'tcx> {
  pub stmts: Vec<RStmt<'tcx>>,
  pub expr: Option<RExpr<'tcx>>,
}

#[derive(Clone, Debug)]
pub struct RStmt<'tcx> {
  pub kind: RStmtKind<'tcx>,
}

#[derive(Clone, Debug)]
pub enum RStmtKind<'tcx> {
  Expr {
    expr: RExpr<'tcx>,
  },
  Let {
    pattern: Box<RPat<'tcx>>,
    initializer: Option<RExpr<'tcx>>,
    else_block: Option<RBlock<'tcx>>,
    span: Span,
  },
}
