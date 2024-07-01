// rustc crates
use rustc_middle::thir::*;

// std crates
// Own crates
use crate::thir::*;

pub fn get_reduced_thir(thir: Thir) -> RThir {
  let mut reducer = ThirReducer::new(thir);
  reducer.reduce();
  reducer.reduced_thir
}

#[derive(Debug)]
struct ThirReducer<'tcx> {
  thir: Thir<'tcx>,
  reduced_thir: RThir<'tcx>,
}

impl<'tcx> ThirReducer<'tcx> {
  fn new(thir: Thir<'tcx>) -> Self { Self { thir, reduced_thir: RThir::new() } }

  fn reduce(&mut self) {
    let new_params = self.reduce_params();
    self.reduced_thir.set_params(new_params);
    let new_body = self.reduce_body();
    self.reduced_thir.set_body(new_body);
  }

  fn reduce_params(&self) -> Vec<RParam<'tcx>> {
    let mut new_params: Vec<RParam<'tcx>> = Vec::new();
    for param in self.thir.params.iter() {
      new_params.push(self.reduce_param(param));
    }
    new_params
  }

  fn reduce_param(&self, param: &Param<'tcx>) -> RParam<'tcx> {
    let Param { pat, .. } = param;
    RParam {
      pat: if let Some(pat) = pat { Some(Box::new(self.reduce_pattern(pat))) } else { None },
    }
  }

  fn reduce_pattern(&self, pat: &Box<Pat<'tcx>>) -> RPat<'tcx> {
    let Pat { span, kind, .. } = &**pat;
    RPat { kind: self.reduce_pattern_kind(kind), span: *span }
  }

  fn reduce_pattern_kind(&self, pat_kind: &PatKind<'tcx>) -> RPatKind<'tcx> {
    let boxed_slice_to_new = |boxed_slice: &Box<[Box<Pat<'tcx>>]>| {
      boxed_slice
        .iter()
        .map(|pat| Box::new(self.reduce_pattern(pat)))
        .collect::<Vec<Box<RPat<'tcx>>>>()
        .into_boxed_slice()
    };

    match pat_kind {
      PatKind::Wild => RPatKind::Wild,
      PatKind::AscribeUserType { ascription, subpattern } => RPatKind::AscribeUserType {
        ascription: ascription.clone(),
        subpattern: Box::new(self.reduce_pattern(subpattern)),
      },
      PatKind::Binding { name, mode, var, ty, subpattern, is_primary } => RPatKind::Binding {
        name: *name,
        mode: *mode,
        var: *var,
        ty: *ty,
        subpattern: if let Some(pat) = subpattern {
          Some(Box::new(self.reduce_pattern(pat)))
        } else {
          None
        },
        is_primary: *is_primary,
      },
      PatKind::Deref { subpattern } => {
        RPatKind::Deref { subpattern: Box::new(self.reduce_pattern(subpattern)) }
      }
      PatKind::DerefPattern { subpattern, mutability } => RPatKind::DerefPattern {
        subpattern: Box::new(self.reduce_pattern(subpattern)),
        mutability: *mutability,
      },
      PatKind::Constant { value } => RPatKind::Constant { value: *value },
      PatKind::Range(patrange) => RPatKind::Range(patrange.clone()),
      PatKind::Or { pats } => RPatKind::Or { pats: boxed_slice_to_new(pats) },
      PatKind::Never => RPatKind::Never,
      PatKind::Error(err) => RPatKind::Error(*err),
      _ => unimplemented!(),
    }
  }

  fn reduce_body(&self) -> Option<RExpr<'tcx>> {
    let expr_id = ExprId::from_usize(self.thir.exprs.len() - 1);
    Some(self.reduce_expr(&expr_id))
  }

  fn reduce_expr(&self, expr_id: &ExprId) -> RExpr<'tcx> {
    let expr = &self.thir[*expr_id];
    let rexprkind = self.reduce_expr_kind(&expr.kind);
    RExpr::new(rexprkind, expr.span)
  }

  fn reduce_expr_kind(&self, expr_kind: &ExprKind<'tcx>) -> RExprKind<'tcx> {
    use rustc_middle::thir::ExprKind::*;
    let unwrap_option = |value: &Option<ExprId>| {
      if let Some(expr_id) = value {
        Some(self.reduce_expr(expr_id))
      } else {
        None
      }
    };

    match expr_kind {
      Scope { value, .. } => self.handle_scope(value),
      Box { value } => RExprKind::Box { value: self.reduce_expr(value) },
      If { cond, then, else_opt, .. } => RExprKind::If {
        cond: self.reduce_expr(cond),
        then: self.reduce_expr(then),
        else_opt: unwrap_option(else_opt),
      },
      Call { ty, fun, args, from_hir_call, fn_span } => RExprKind::Call {
        ty: *ty,
        fun: self.reduce_expr(fun),
        args: args.iter().map(|arg| self.reduce_expr(arg)).collect(),
        from_hir_call: *from_hir_call,
        fn_span: *fn_span,
      },
      Deref { arg } => RExprKind::Deref { arg: self.reduce_expr(arg) },
      Binary { op, lhs, rhs } => {
        RExprKind::Binary { op: *op, lhs: self.reduce_expr(lhs), rhs: self.reduce_expr(rhs) }
      }
      LogicalOp { op, lhs, rhs } => {
        RExprKind::LogicalOp { op: *op, lhs: self.reduce_expr(lhs), rhs: self.reduce_expr(rhs) }
      }
      Unary { op, arg } => RExprKind::Unary { op: *op, arg: self.reduce_expr(arg) },
      Cast { source } => RExprKind::Cast { source: self.reduce_expr(source) },
      Use { source } => self.handle_use(source),
      NeverToAny { source } => self.handle_never_to_any(source),
      PointerCoercion { cast, source } => {
        RExprKind::PointerCoercion { cast: *cast, source: self.reduce_expr(source) }
      }
      Loop { body } => RExprKind::Loop { body: self.reduce_expr(body) },
      Let { expr, pat } => RExprKind::Let { expr: self.reduce_expr(expr), pat: pat.clone() },
      Match { scrutinee, arms, .. } => RExprKind::Match {
        scrutinee: self.reduce_expr(scrutinee),
        arms: arms.iter().map(|arm| self.handle_arm(arm)).collect(),
      },
      Block { block } => RExprKind::Block { block: self.handle_block(block) },
      Assign { lhs, rhs } => {
        RExprKind::Assign { lhs: self.reduce_expr(lhs), rhs: self.reduce_expr(rhs) }
      }
      AssignOp { op, lhs, rhs } => {
        RExprKind::AssignOp { op: *op, lhs: self.reduce_expr(lhs), rhs: self.reduce_expr(rhs) }
      }
      Field { lhs, variant_index, name } => {
        RExprKind::Field { lhs: self.reduce_expr(lhs), variant_index: *variant_index, name: *name }
      }
      Index { lhs, index } => {
        RExprKind::Index { lhs: self.reduce_expr(lhs), index: self.reduce_expr(index) }
      }
      VarRef { id } => RExprKind::VarRef { id: *id },
      UpvarRef { closure_def_id, var_hir_id } => {
        RExprKind::UpvarRef { closure_def_id: *closure_def_id, var_hir_id: *var_hir_id }
      }
      Borrow { borrow_kind, arg } => {
        RExprKind::Borrow { borrow_kind: *borrow_kind, arg: self.reduce_expr(arg) }
      }
      Break { label, value } => RExprKind::Break { label: *label, value: unwrap_option(value) },
      Continue { label } => RExprKind::Continue { label: *label },
      Return { value } => RExprKind::Return { value: unwrap_option(value) },
      Repeat { value, count } => {
        RExprKind::Repeat { value: self.reduce_expr(value), count: *count }
      }
      Array { fields } => {
        RExprKind::Array { fields: fields.iter().map(|f| self.reduce_expr(f)).collect() }
      }
      Tuple { fields } => {
        RExprKind::Tuple { fields: fields.iter().map(|f| self.reduce_expr(f)).collect() }
      }
      PlaceTypeAscription { source, user_ty } => RExprKind::PlaceTypeAscription {
        source: self.reduce_expr(source),
        user_ty: user_ty.clone(),
      },
      ValueTypeAscription { source, user_ty } => RExprKind::ValueTypeAscription {
        source: self.reduce_expr(source),
        user_ty: user_ty.clone(),
      },
      Literal { lit, neg } => RExprKind::Literal { lit: *lit, neg: *neg },
      NonHirLiteral { lit, user_ty } => {
        RExprKind::NonHirLiteral { lit: *lit, user_ty: user_ty.clone() }
      }
      ZstLiteral { user_ty } => RExprKind::ZstLiteral { user_ty: user_ty.clone() },
      NamedConst { def_id, args, user_ty } => {
        RExprKind::NamedConst { def_id: *def_id, args: *args, user_ty: user_ty.clone() }
      }
      ConstParam { param, def_id } => RExprKind::ConstParam { param: *param, def_id: *def_id },
      _ => unimplemented!(),
    }
  }

  fn handle_scope(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    let scope = &self.thir[*expr_id];
    self.reduce_expr_kind(&scope.kind)
  }

  fn handle_use(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    let use_expr = &self.thir[*expr_id];
    self.reduce_expr_kind(&use_expr.kind)
  }

  fn handle_never_to_any(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    let never_to_any = &self.thir[*expr_id];
    self.reduce_expr_kind(&never_to_any.kind)
  }

  fn handle_arm(&self, arm_id: &ArmId) -> RArm<'tcx> {
    let arm = &self.thir.arms[*arm_id];
    RArm {
      pattern: self.reduce_pattern(&arm.pattern),
      guard: if let Some(expr_id) = arm.guard { Some(self.reduce_expr(&expr_id)) } else { None },
      body: self.reduce_expr(&arm.body),
      span: arm.span,
    }
  }

  fn handle_block(&self, block_id: &BlockId) -> RBlock<'tcx> {
    let block = &self.thir.blocks[*block_id];

    let mut stmtv = Vec::new();
    for stmt in block.stmts.iter() {
      stmtv.push(self.handle_stmt(*stmt));
    }

    RBlock {
      stmts: stmtv,
      expr: if let Some(expr_id) = block.expr { Some(self.reduce_expr(&expr_id)) } else { None },
    }
  }

  fn handle_stmt(&self, stmt_id: StmtId) -> RStmt<'tcx> {
    let Stmt { kind } = &self.thir.stmts[stmt_id];
    match kind {
      StmtKind::Expr { expr, .. } => {
        RStmt { kind: RStmtKind::Expr { expr: self.reduce_expr(expr) } }
      }
      StmtKind::Let { pattern, initializer, else_block, span, .. } => RStmt {
        kind: RStmtKind::Let {
          pattern: Box::new(self.reduce_pattern(pattern)),
          initializer: if let Some(expr_id) = initializer {
            Some(self.reduce_expr(&expr_id))
          } else {
            None
          },
          else_block: if let Some(block_id) = else_block {
            Some(self.handle_block(&block_id))
          } else {
            None
          },
          span: *span,
        },
      },
    }
  }
}
