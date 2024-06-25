// rustc crates
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

  pub fn into_buffer(self) -> String { self.fmt }

  fn indent(&mut self, level: usize) {
    for _ in 0..level {
      self.fmt.push_str(INDENT);
    }
  }

  pub fn print(&mut self) {
    print_indented!(self, "params: [", 0);

    for param in self.rthir.params.iter() {
      self.print_param(param, 1);
    }

    print_indented!(self, "]", 0);

    print_indented!(self, "body:", 0);
    if let Some(body) = &self.rthir.body {
      self.print_expr(&body, 1);
    } else {
      print_indented!(self, "None", 1);
    }
  }

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

  fn print_pat(&mut self, pat: &RPat<'tcx>, depth_lvl: usize) {
    let RPat { kind, span } = pat;

    print_indented!(self, "Pat {", depth_lvl);
    print_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    self.print_pat_kind(kind, depth_lvl + 1);
    print_indented!(self, "}", depth_lvl);
  }

  fn print_pat_kind(&mut self, pat_kind: &RPatKind<'tcx>, depth_lvl: usize) {
    print_indented!(self, "kind: PatKind {", depth_lvl);

    match pat_kind {
      RPatKind::Never => {
        print_indented!(self, "Never", depth_lvl + 1);
      }
      RPatKind::AscribeUserType { ascription, subpattern } => {
        print_indented!(self, "AscribeUserType: {", depth_lvl + 1);
        print_indented!(self, format!("ascription: {:?}", ascription), depth_lvl + 2);
        print_indented!(self, "subpattern: ", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 3);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Binding { name, mode, var, ty, subpattern, is_primary } => {
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
      RPatKind::Deref { subpattern } => {
        print_indented!(self, "Deref { ", depth_lvl + 1);
        print_indented!(self, "subpattern:", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::DerefPattern { subpattern, .. } => {
        print_indented!(self, "DerefPattern { ", depth_lvl + 1);
        print_indented!(self, "subpattern:", depth_lvl + 2);
        self.print_pat(subpattern, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Constant { value } => {
        print_indented!(self, "Constant {", depth_lvl + 1);
        print_indented!(self, format!("value: {:?}", value), depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Range(pat_range) => {
        print_indented!(self, format!("Range ( {:?} )", pat_range), depth_lvl + 1);
      }
      RPatKind::Or { pats } => {
        print_indented!(self, "Or {", depth_lvl + 1);
        print_indented!(self, "pats: [", depth_lvl + 2);
        for pat in pats.iter() {
          self.print_pat(pat, depth_lvl + 3);
        }
        print_indented!(self, "]", depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RPatKind::Error(_) => {
        print_indented!(self, "Error", depth_lvl + 1);
      }
      _ => unimplemented!(),
    }

    print_indented!(self, "}", depth_lvl);
  }

  fn print_expr(&mut self, expr: &RExpr<'tcx>, depth_lvl: usize) {
    let RExpr { span, kind } = expr;
    print_indented!(self, "Expr {", depth_lvl);
    print_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    print_indented!(self, "kind: ", depth_lvl + 1);
    self.print_expr_kind(kind, depth_lvl + 2);
    print_indented!(self, "}", depth_lvl);
  }

  fn print_expr_kind(&mut self, expr_kind: &RExprKind<'tcx>, depth_lvl: usize) {
    use RExprKind::*;

    match expr_kind {
      Box { value } => {
        print_indented!(self, "Box {", depth_lvl);
        self.print_expr(value, depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      If { cond, then, else_opt } => {
        print_indented!(self, "If {", depth_lvl);
        print_indented!(self, "cond:", depth_lvl + 1);
        self.print_expr(cond, depth_lvl + 2);
        print_indented!(self, "then:", depth_lvl + 1);
        self.print_expr(then, depth_lvl + 2);

        if let Some(else_expr) = else_opt {
          print_indented!(self, "else:", depth_lvl + 1);
          self.print_expr(else_expr, depth_lvl + 2);
        }

        print_indented!(self, "}", depth_lvl);
      }
      Call { fun, args, ty, from_hir_call, fn_span } => {
        print_indented!(self, "Call {", depth_lvl);
        print_indented!(self, format!("ty: {:?}", ty), depth_lvl + 1);
        print_indented!(self, format!("from_hir_call: {}", from_hir_call), depth_lvl + 1);
        print_indented!(self, format!("fn_span: {:?}", fn_span), depth_lvl + 1);
        print_indented!(self, "fun:", depth_lvl + 1);
        self.print_expr(fun, depth_lvl + 2);

        if args.len() > 0 {
          print_indented!(self, "args: [", depth_lvl + 1);
          for arg in args.iter() {
            self.print_expr(arg, depth_lvl + 2);
          }
          print_indented!(self, "]", depth_lvl + 1);
        } else {
          print_indented!(self, "args: []", depth_lvl + 1);
        }

        print_indented!(self, "}", depth_lvl);
      }
      Deref { arg } => {
        print_indented!(self, "Deref {", depth_lvl);
        self.print_expr(arg, depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Binary { op, lhs, rhs } => {
        print_indented!(self, "Binary {", depth_lvl);
        print_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "rhs:", depth_lvl + 1);
        self.print_expr(rhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      LogicalOp { op, lhs, rhs } => {
        print_indented!(self, "LogicalOp {", depth_lvl);
        print_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "rhs:", depth_lvl + 1);
        self.print_expr(rhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Unary { op, arg } => {
        print_indented!(self, "Unary {", depth_lvl);
        print_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        print_indented!(self, "arg:", depth_lvl + 1);
        self.print_expr(arg, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Cast { source } => {
        print_indented!(self, "Cast {", depth_lvl);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Use { source } => {
        print_indented!(self, "Use {", depth_lvl);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      NeverToAny { source } => {
        print_indented!(self, "NeverToAny {", depth_lvl);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      PointerCoercion { cast, source } => {
        print_indented!(self, "Pointer {", depth_lvl);
        print_indented!(self, format!("cast: {:?}", cast), depth_lvl + 1);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Loop { body } => {
        print_indented!(self, "Loop (", depth_lvl);
        print_indented!(self, "body:", depth_lvl + 1);
        self.print_expr(body, depth_lvl + 2);
        print_indented!(self, ")", depth_lvl);
      }
      Let { expr, pat } => {
        print_indented!(self, "Let {", depth_lvl);
        print_indented!(self, "expr:", depth_lvl + 1);
        self.print_expr(expr, depth_lvl + 2);
        print_indented!(self, format!("pat: {:?}", pat), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Match { scrutinee, arms, .. } => {
        print_indented!(self, "Match {", depth_lvl);
        print_indented!(self, "scrutinee:", depth_lvl + 1);
        self.print_expr(scrutinee, depth_lvl + 2);

        print_indented!(self, "arms: [", depth_lvl + 1);
        for arm_id in arms.iter() {
          self.print_arm(arm_id, depth_lvl + 2);
        }
        print_indented!(self, "]", depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Block { block } => self.print_block(block, depth_lvl),
      Assign { lhs, rhs } => {
        print_indented!(self, "Assign {", depth_lvl);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "rhs:", depth_lvl + 1);
        self.print_expr(rhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      AssignOp { op, lhs, rhs } => {
        print_indented!(self, "AssignOp {", depth_lvl);
        print_indented!(self, format!("op: {:?}", op), depth_lvl + 1);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "rhs:", depth_lvl + 1);
        self.print_expr(rhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Field { lhs, variant_index, name } => {
        print_indented!(self, "Field {", depth_lvl);
        print_indented!(self, format!("variant_index: {:?}", variant_index), depth_lvl + 1);
        print_indented!(self, format!("name: {:?}", name), depth_lvl + 1);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Index { lhs, index } => {
        print_indented!(self, "Index {", depth_lvl);
        print_indented!(self, format!("index: {:?}", index), depth_lvl + 1);
        print_indented!(self, "lhs:", depth_lvl + 1);
        self.print_expr(lhs, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      VarRef { id } => {
        print_indented!(self, "VarRef {", depth_lvl);
        print_indented!(self, format!("id: {:?}", id), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      UpvarRef { closure_def_id, var_hir_id } => {
        print_indented!(self, "UpvarRef {", depth_lvl);
        print_indented!(self, format!("closure_def_id: {:?}", closure_def_id), depth_lvl + 1);
        print_indented!(self, format!("var_hir_id: {:?}", var_hir_id), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Borrow { borrow_kind, arg } => {
        print_indented!(self, "Borrow (", depth_lvl);
        print_indented!(self, format!("borrow_kind: {:?}", borrow_kind), depth_lvl + 1);
        print_indented!(self, "arg:", depth_lvl + 1);
        self.print_expr(arg, depth_lvl + 2);
        print_indented!(self, ")", depth_lvl);
      }
      Break { label, value } => {
        print_indented!(self, "Break (", depth_lvl);
        print_indented!(self, format!("label: {:?}", label), depth_lvl + 1);

        if let Some(value) = value {
          print_indented!(self, "value:", depth_lvl + 1);
          self.print_expr(value, depth_lvl + 2);
        }

        print_indented!(self, ")", depth_lvl);
      }
      Continue { label } => {
        print_indented!(self, "Continue {", depth_lvl);
        print_indented!(self, format!("label: {:?}", label), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Return { value } => {
        print_indented!(self, "Return {", depth_lvl);
        print_indented!(self, "value:", depth_lvl + 1);

        if let Some(value) = value {
          self.print_expr(value, depth_lvl + 2);
        }

        print_indented!(self, "}", depth_lvl);
      }
      Repeat { value, count } => {
        print_indented!(self, "Repeat {", depth_lvl);
        print_indented!(self, format!("count: {:?}", count), depth_lvl + 1);
        print_indented!(self, "value:", depth_lvl + 1);
        self.print_expr(value, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Array { fields } => {
        print_indented!(self, "Array {", depth_lvl);
        print_indented!(self, "fields: [", depth_lvl + 1);
        for field in fields.iter() {
          self.print_expr(field, depth_lvl + 2);
        }
        print_indented!(self, "]", depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      Tuple { fields } => {
        print_indented!(self, "Tuple {", depth_lvl);
        print_indented!(self, "fields: [", depth_lvl + 1);
        for field_id in fields.iter() {
          self.print_expr(field_id, depth_lvl + 2);
        }
        print_indented!(self, "]", depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      PlaceTypeAscription { source, user_ty } => {
        print_indented!(self, "PlaceTypeAscription {", depth_lvl);
        print_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      ValueTypeAscription { source, user_ty } => {
        print_indented!(self, "ValueTypeAscription {", depth_lvl);
        print_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        print_indented!(self, "source:", depth_lvl + 1);
        self.print_expr(source, depth_lvl + 2);
        print_indented!(self, "}", depth_lvl);
      }
      Literal { lit, neg } => {
        print_indented!(self, format!("Literal( lit: {:?}, neg: {:?})\n", lit, neg), depth_lvl);
      }
      NonHirLiteral { lit, user_ty } => {
        print_indented!(self, "NonHirLiteral {", depth_lvl);
        print_indented!(self, format!("lit: {:?}", lit), depth_lvl + 1);
        print_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      ZstLiteral { user_ty } => {
        print_indented!(self, format!("ZstLiteral(user_ty: {:?})", user_ty), depth_lvl);
      }
      NamedConst { def_id, args, user_ty } => {
        print_indented!(self, "NamedConst {", depth_lvl);
        print_indented!(self, format!("def_id: {:?}", def_id), depth_lvl + 1);
        print_indented!(self, format!("user_ty: {:?}", user_ty), depth_lvl + 1);
        print_indented!(self, format!("args: {:?}", args), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      ConstParam { param, def_id } => {
        print_indented!(self, "ConstParam {", depth_lvl);
        print_indented!(self, format!("def_id: {:?}", def_id), depth_lvl + 1);
        print_indented!(self, format!("param: {:?}", param), depth_lvl + 1);
        print_indented!(self, "}", depth_lvl);
      }
      _ => unimplemented!(),
    }
  }

  fn print_arm(&mut self, arm: &RArm<'tcx>, depth_lvl: usize) {
    let RArm { pattern, guard, body, span } = arm;

    print_indented!(self, "pattern: ", depth_lvl + 1);
    self.print_pat(pattern, depth_lvl + 2);

    if let Some(guard) = guard {
      print_indented!(self, "guard: ", depth_lvl + 1);
      self.print_expr(guard, depth_lvl + 2);
    } else {
      print_indented!(self, "guard: None", depth_lvl + 1);
    }

    print_indented!(self, "body: ", depth_lvl + 1);
    self.print_expr(body, depth_lvl + 2);
    print_indented!(self, format!("span: {:?}", span), depth_lvl + 1);
    print_indented!(self, "}", depth_lvl);
  }

  fn print_block(&mut self, block: &RBlock<'tcx>, depth_lvl: usize) {
    let RBlock { expr, stmts } = block;

    print_indented!(self, "Block {", depth_lvl);

    if stmts.len() > 0 {
      print_indented!(self, "stmts: [", depth_lvl + 1);
      for stmt in stmts.iter() {
        self.print_stmt(stmt, depth_lvl + 2);
      }
      print_indented!(self, "]", depth_lvl + 1);
    } else {
      print_indented!(self, "stmts: []", depth_lvl + 1);
    }

    if let Some(expr) = expr {
      print_indented!(self, "expr:", depth_lvl + 1);
      self.print_expr(expr, depth_lvl + 2);
    } else {
      print_indented!(self, "expr: []", depth_lvl + 1);
    }

    print_indented!(self, "}", depth_lvl);
  }

  fn print_stmt(&mut self, stmt: &RStmt<'tcx>, depth_lvl: usize) {
    let RStmt { kind } = stmt;

    print_indented!(self, "Stmt {", depth_lvl);

    match kind {
      RStmtKind::Expr { expr } => {
        print_indented!(self, "kind: Expr {", depth_lvl + 1);
        print_indented!(self, "expr:", depth_lvl + 2);
        self.print_expr(expr, depth_lvl + 3);
        print_indented!(self, "}", depth_lvl + 1);
      }
      RStmtKind::Let { pattern, initializer, else_block, span } => {
        print_indented!(self, "kind: Let {", depth_lvl + 1);

        print_indented!(self, "pattern: ", depth_lvl + 2);
        self.print_pat(pattern, depth_lvl + 3);
        print_indented!(self, ",", depth_lvl + 2);

        if let Some(init) = initializer {
          print_indented!(self, "initializer: Some(", depth_lvl + 2);
          self.print_expr(init, depth_lvl + 3);
          print_indented!(self, ")", depth_lvl + 2);
        } else {
          print_indented!(self, "initializer: None", depth_lvl + 2);
        }

        if let Some(else_block) = else_block {
          print_indented!(self, "else_block: Some(", depth_lvl + 2);
          self.print_block(else_block, depth_lvl + 3);
          print_indented!(self, ")", depth_lvl + 2);
        } else {
          print_indented!(self, "else_block: None", depth_lvl + 2);
        }

        print_indented!(self, format!("span: {:?}", span), depth_lvl + 2);
        print_indented!(self, "}", depth_lvl + 1);
      }
    }

    print_indented!(self, "}", depth_lvl);
  }
}
