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
use std::fmt::{self, Write};
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
    let mut formatter = RThirFormatter::new(self);
    formatter.format();
    write!(f, "{}", formatter.get_formated_rthir())
  }
}

struct RThirFormatter<'a, 'tcx> {
  rthir: &'a RThir<'tcx>,
  fmt: String,
}

const INDENT: &str = "  ";

macro_rules! format_indented {
  ($writer:ident, $s:expr, $indent_lvl:expr) => {
    $writer.indent($indent_lvl);
    writeln!($writer, "{}", $s).expect("unable to write to RThirFormatter");
  };
}

impl<'a, 'tcx> Write for RThirFormatter<'a, 'tcx> {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    self.fmt.push_str(s);
    Ok(())
  }
}

impl<'a, 'tcx> RThirFormatter<'a, 'tcx> {
  pub fn new(rthir: &'a RThir<'tcx>) -> Self { Self { rthir, fmt: String::new() } }

  pub fn get_formated_rthir(self) -> String { self.fmt }

  fn indent(&mut self, level: usize) {
    for _ in 0..level {
      self.fmt.push_str(INDENT);
    }
  }

  pub fn format(&mut self) {
    format_indented!(self, "params: [", 0);

    for param in self.rthir.params.iter() {
      self.format_param(param, 1);
    }

    format_indented!(self, "]", 0);

    format_indented!(self, "body:", 0);
    if let Some(body) = &self.rthir.body {
      self.format_expr(&body, 1);
    } else {
      format_indented!(self, "None", 1);
    }
  }

  fn format_param(&mut self, param: &RParam<'tcx>, depth_lvl: usize) {
    let RParam { pat } = param;

    format_indented!(self, "Param {", depth_lvl);

    if let Some(pat) = pat {
      format_indented!(self, "param: Some(", depth_lvl + 1);
      self.format_pat(pat, depth_lvl + 2);
      format_indented!(self, ")", depth_lvl + 1);
    } else {
      format_indented!(self, "param: None", depth_lvl + 1);
    }

    format_indented!(self, "}", depth_lvl);
  }

  fn format_pat(&mut self, pat: &RPat<'tcx>, depth_lvl: usize) {
    let RPat { kind, span } = pat;

    format_indented!(self, "Pat {", depth_lvl);
    format_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    self.format_pat_kind(kind, depth_lvl + 1);
    format_indented!(self, "}", depth_lvl);
  }

  fn format_pat_kind(&mut self, pat_kind: &RPatKind<'tcx>, depth_lvl: usize) {
    format_indented!(self, "kind: PatKind {", depth_lvl);

    match pat_kind {
      RPatKind::Never => {
        format_indented!(self, "Never", depth_lvl + 1);
      }
      RPatKind::AscribeUserType { ascription, subpattern } => {
        format_indented!(self, "AscribeUserType: {", depth_lvl + 1);
        format_indented!(self, format!("ascription: {:?}", ascription), depth_lvl + 2);
        format_indented!(self, "subpattern: ", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 3);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Binding { name, mode, var, ty, subpattern, is_primary } => {
        format_indented!(self, "Binding {", depth_lvl + 1);
        format_indented!(self, format!("name: {:?}", name), depth_lvl + 2);
        format_indented!(self, format!("mode: {:?}", mode), depth_lvl + 2);
        format_indented!(self, format!("var: {:?}", var), depth_lvl + 2);
        format_indented!(self, format!("ty: {:?}", ty), depth_lvl + 2);
        format_indented!(self, format!("is_primary: {:?}", is_primary), depth_lvl + 2);

        if let Some(subpattern) = subpattern {
          format_indented!(self, "subpattern: Some( ", depth_lvl + 2);
          self.format_pat(subpattern, depth_lvl + 3);
          format_indented!(self, ")", depth_lvl + 2);
        } else {
          format_indented!(self, "subpattern: None", depth_lvl + 2);
        }

        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Deref { subpattern } => {
        format_indented!(self, "Deref { ", depth_lvl + 1);
        format_indented!(self, "subpattern:", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::DerefPattern { subpattern, mutability } => {
        format_indented!(self, "DerefPattern { ", depth_lvl + 1);
        format_indented!(self, format!("mutability: {:?}", mutability), depth_lvl + 2);
        format_indented!(self, "subpattern:", depth_lvl + 2);
        self.format_pat(subpattern, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Constant { value } => {
        format_indented!(self, "Constant {", depth_lvl + 1);
        format_indented!(self, format!("value: {:?}", value), depth_lvl + 2);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Range(pat_range) => {
        format_indented!(self, format!("Range ( {:?} )", pat_range), depth_lvl + 1);
      }
      RPatKind::Or { pats } => {
        format_indented!(self, "Or {", depth_lvl + 1);
        format_indented!(self, "pats: [", depth_lvl + 2);
        for pat in pats.iter() {
          self.format_pat(pat, depth_lvl + 3);
        }
        format_indented!(self, "]", depth_lvl + 2);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Error(_) => {
        format_indented!(self, "Error", depth_lvl + 1);
      }
    }

    format_indented!(self, "}", depth_lvl);
  }

  fn format_expr(&mut self, expr: &RExpr<'tcx>, depth_lvl: usize) {
    let RExpr { span, kind } = expr;
    format_indented!(self, "Expr {", depth_lvl);
    format_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    format_indented!(self, "kind: ", depth_lvl + 1);
    self.format_expr_kind(kind, depth_lvl + 2);
    format_indented!(self, "}", depth_lvl);
  }

  fn format_expr_kind(&mut self, expr_kind: &RExprKind<'tcx>, depth_lvl: usize) {
    use RExprKind::*;

    match expr_kind {
      Box { value } => {
        format_indented!(self, "Box {", depth_lvl);
        self.format_expr(value, depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      If { cond, then, else_opt } => {
        format_indented!(self, "If {", depth_lvl);
        format_indented!(self, "cond:", depth_lvl + 1);
        self.format_expr(cond, depth_lvl + 2);
        format_indented!(self, "then:", depth_lvl + 1);
        self.format_expr(then, depth_lvl + 2);

        if let Some(else_expr) = else_opt {
          format_indented!(self, "else:", depth_lvl + 1);
          self.format_expr(else_expr, depth_lvl + 2);
        }

        format_indented!(self, "}", depth_lvl);
      }
      Call { fun, args, ty, from_hir_call, fn_span } => {
        format_indented!(self, "Call {", depth_lvl);
        format_indented!(self, format!("ty: {:?}", ty), depth_lvl + 1);
        format_indented!(self, format!("from_hir_call: {}", from_hir_call), depth_lvl + 1);
        format_indented!(self, format!("fn_span: {:?}", fn_span), depth_lvl + 1);
        format_indented!(self, "fun:", depth_lvl + 1);
        self.format_expr(fun, depth_lvl + 2);

        if args.len() > 0 {
          format_indented!(self, "args: [", depth_lvl + 1);
          for arg in args.iter() {
            self.format_expr(arg, depth_lvl + 2);
          }
          format_indented!(self, "]", depth_lvl + 1);
        } else {
          format_indented!(self, "args: []", depth_lvl + 1);
        }

        format_indented!(self, "}", depth_lvl);
      }
      Deref { arg } => {
        format_indented!(self, "Deref {", depth_lvl);
        self.format_expr(arg, depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Binary { op, lhs, rhs } => {
        format_indented!(self, "Binary {", depth_lvl);
        format_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      LogicalOp { op, lhs, rhs } => {
        format_indented!(self, "LogicalOp {", depth_lvl);
        format_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Unary { op, arg } => {
        format_indented!(self, "Unary {", depth_lvl);
        format_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        format_indented!(self, "arg:", depth_lvl + 1);
        self.format_expr(arg, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Cast { source } => {
        format_indented!(self, "Cast {", depth_lvl);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Use { source } => {
        format_indented!(self, "Use {", depth_lvl);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      NeverToAny { source } => {
        format_indented!(self, "NeverToAny {", depth_lvl);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      PointerCoercion { cast, source } => {
        format_indented!(self, "Pointer {", depth_lvl);
        format_indented!(self, format!("cast: {:?}", cast), depth_lvl + 1);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Loop { body } => {
        format_indented!(self, "Loop (", depth_lvl);
        format_indented!(self, "body:", depth_lvl + 1);
        self.format_expr(body, depth_lvl + 2);
        format_indented!(self, ")", depth_lvl);
      }
      Let { expr, pat } => {
        format_indented!(self, "Let {", depth_lvl);
        format_indented!(self, "expr:", depth_lvl + 1);
        self.format_expr(expr, depth_lvl + 2);
        format_indented!(self, format!("pat: {:?}", pat), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Match { scrutinee, arms, .. } => {
        format_indented!(self, "Match {", depth_lvl);
        format_indented!(self, "scrutinee:", depth_lvl + 1);
        self.format_expr(scrutinee, depth_lvl + 2);

        format_indented!(self, "arms: [", depth_lvl + 1);
        for arm_id in arms.iter() {
          self.format_arm(arm_id, depth_lvl + 2);
        }
        format_indented!(self, "]", depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Block { block } => self.format_block(block, depth_lvl),
      Assign { lhs, rhs } => {
        format_indented!(self, "Assign {", depth_lvl);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      AssignOp { op, lhs, rhs } => {
        format_indented!(self, "AssignOp {", depth_lvl);
        format_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "rhs:", depth_lvl + 1);
        self.format_expr(rhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Field { lhs, variant_index, name } => {
        format_indented!(self, "Field {", depth_lvl);
        format_indented!(self, format!("variant_index: {:?}", variant_index), depth_lvl + 1);
        format_indented!(self, format!("name: {:?}", name), depth_lvl + 1);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Index { lhs, index } => {
        format_indented!(self, "Index {", depth_lvl);
        format_indented!(self, format!("index: {:?}", index), depth_lvl + 1);
        format_indented!(self, "lhs:", depth_lvl + 1);
        self.format_expr(lhs, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      VarRef { id } => {
        format_indented!(self, "VarRef {", depth_lvl);
        format_indented!(self, format!("id: {:?}", id), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      UpvarRef { closure_def_id, var_hir_id } => {
        format_indented!(self, "UpvarRef {", depth_lvl);
        format_indented!(self, format!("closure_def_id: {:?}", closure_def_id), depth_lvl + 1);
        format_indented!(self, format!("var_hir_id: {:?}", var_hir_id), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Borrow { borrow_kind, arg } => {
        format_indented!(self, "Borrow (", depth_lvl);
        format_indented!(self, format!("borrow_kind: {:?}", borrow_kind), depth_lvl + 1);
        format_indented!(self, "arg:", depth_lvl + 1);
        self.format_expr(arg, depth_lvl + 2);
        format_indented!(self, ")", depth_lvl);
      }
      Break { label, value } => {
        format_indented!(self, "Break (", depth_lvl);
        format_indented!(self, format!("label: {:?}", label), depth_lvl + 1);

        if let Some(value) = value {
          format_indented!(self, "value:", depth_lvl + 1);
          self.format_expr(value, depth_lvl + 2);
        }

        format_indented!(self, ")", depth_lvl);
      }
      Continue { label } => {
        format_indented!(self, "Continue {", depth_lvl);
        format_indented!(self, format!("label: {:?}", label), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Return { value } => {
        format_indented!(self, "Return {", depth_lvl);
        format_indented!(self, "value:", depth_lvl + 1);

        if let Some(value) = value {
          self.format_expr(value, depth_lvl + 2);
        }

        format_indented!(self, "}", depth_lvl);
      }
      Repeat { value, count } => {
        format_indented!(self, "Repeat {", depth_lvl);
        format_indented!(self, format!("count: {:?}", count), depth_lvl + 1);
        format_indented!(self, "value:", depth_lvl + 1);
        self.format_expr(value, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Array { fields } => {
        format_indented!(self, "Array {", depth_lvl);
        format_indented!(self, "fields: [", depth_lvl + 1);
        for field in fields.iter() {
          self.format_expr(field, depth_lvl + 2);
        }
        format_indented!(self, "]", depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      Tuple { fields } => {
        format_indented!(self, "Tuple {", depth_lvl);
        format_indented!(self, "fields: [", depth_lvl + 1);
        for field_id in fields.iter() {
          self.format_expr(field_id, depth_lvl + 2);
        }
        format_indented!(self, "]", depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      PlaceTypeAscription { source, user_ty } => {
        format_indented!(self, "PlaceTypeAscription {", depth_lvl);
        format_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      ValueTypeAscription { source, user_ty } => {
        format_indented!(self, "ValueTypeAscription {", depth_lvl);
        format_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        format_indented!(self, "source:", depth_lvl + 1);
        self.format_expr(source, depth_lvl + 2);
        format_indented!(self, "}", depth_lvl);
      }
      Literal { lit, neg } => {
        format_indented!(self, format!("Literal( lit: {:?}, neg: {:?})\n", lit, neg), depth_lvl);
      }
      NonHirLiteral { lit, user_ty } => {
        format_indented!(self, "NonHirLiteral {", depth_lvl);
        format_indented!(self, format!("lit: {:?}", lit), depth_lvl + 1);
        format_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      ZstLiteral { user_ty } => {
        format_indented!(self, format!("ZstLiteral(user_ty: {:?})", user_ty), depth_lvl);
      }
      NamedConst { def_id, args, user_ty } => {
        format_indented!(self, "NamedConst {", depth_lvl);
        format_indented!(self, format!("def_id: {:?}", def_id), depth_lvl + 1);
        format_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        format_indented!(self, format!("args: {:?}", args), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
      ConstParam { param, def_id } => {
        format_indented!(self, "ConstParam {", depth_lvl);
        format_indented!(self, format!("def_id: {:?}", def_id), depth_lvl + 1);
        format_indented!(self, format!("param: {:?}", param), depth_lvl + 1);
        format_indented!(self, "}", depth_lvl);
      }
    }
  }

  fn format_arm(&mut self, arm: &RArm<'tcx>, depth_lvl: usize) {
    let RArm { pattern, guard, body, span } = arm;

    format_indented!(self, "pattern: ", depth_lvl + 1);
    self.format_pat(pattern, depth_lvl + 2);

    if let Some(guard) = guard {
      format_indented!(self, "guard: ", depth_lvl + 1);
      self.format_expr(guard, depth_lvl + 2);
    } else {
      format_indented!(self, "guard: None", depth_lvl + 1);
    }

    format_indented!(self, "body: ", depth_lvl + 1);
    self.format_expr(body, depth_lvl + 2);
    format_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    format_indented!(self, "}", depth_lvl);
  }

  fn format_block(&mut self, block: &RBlock<'tcx>, depth_lvl: usize) {
    let RBlock { expr, stmts } = block;

    format_indented!(self, "Block {", depth_lvl);

    if stmts.len() > 0 {
      format_indented!(self, "stmts: [", depth_lvl + 1);
      for stmt in stmts.iter() {
        self.format_stmt(stmt, depth_lvl + 2);
      }
      format_indented!(self, "]", depth_lvl + 1);
    } else {
      format_indented!(self, "stmts: []", depth_lvl + 1);
    }

    if let Some(expr) = expr {
      format_indented!(self, "expr:", depth_lvl + 1);
      self.format_expr(expr, depth_lvl + 2);
    } else {
      format_indented!(self, "expr: []", depth_lvl + 1);
    }

    format_indented!(self, "}", depth_lvl);
  }

  fn format_stmt(&mut self, stmt: &RStmt<'tcx>, depth_lvl: usize) {
    let RStmt { kind } = stmt;

    format_indented!(self, "Stmt {", depth_lvl);

    match kind {
      RStmtKind::Expr { expr } => {
        format_indented!(self, "kind: Expr {", depth_lvl + 1);
        format_indented!(self, "expr:", depth_lvl + 2);
        self.format_expr(expr, depth_lvl + 3);
        format_indented!(self, "}", depth_lvl + 1);
      }
      RStmtKind::Let { pattern, initializer, else_block, span } => {
        format_indented!(self, "kind: Let {", depth_lvl + 1);

        format_indented!(self, "pattern: ", depth_lvl + 2);
        self.format_pat(pattern, depth_lvl + 3);
        format_indented!(self, ",", depth_lvl + 2);

        if let Some(init) = initializer {
          format_indented!(self, "initializer: Some(", depth_lvl + 2);
          self.format_expr(init, depth_lvl + 3);
          format_indented!(self, ")", depth_lvl + 2);
        } else {
          format_indented!(self, "initializer: None", depth_lvl + 2);
        }

        if let Some(else_block) = else_block {
          format_indented!(self, "else_block: Some(", depth_lvl + 2);
          self.format_block(else_block, depth_lvl + 3);
          format_indented!(self, ")", depth_lvl + 2);
        } else {
          format_indented!(self, "else_block: None", depth_lvl + 2);
        }

        format_indented!(self, format!("span: {:?}", span), depth_lvl + 2);
        format_indented!(self, "}", depth_lvl + 1);
      }
    }

    format_indented!(self, "}", depth_lvl);
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
