// rustc crates
use rustc_data_structures::steal::Steal;
use rustc_index::IndexVec;
use rustc_middle::thir::*;

// std crates
// Own crates
mod rexpr;
use rexpr::*;

#[derive(Debug)]
pub struct ThirReducer<'a, 'tcx> {
  thir: &'a Thir<'tcx>,
  reduced_thir: Steal<RThir<'tcx>>,
}

impl<'a, 'tcx> ThirReducer<'a, 'tcx> {
  pub fn new(thir: &'a Thir<'tcx>) -> Self { Self { thir, reduced_thir: Steal::new(RThir::new()) } }

  pub fn get_reduced_thir(&mut self) -> RThir<'tcx> { self.reduced_thir.steal() }

  pub fn reduce(&mut self) {
    self.reduced_thir.get_mut().set_params(&self.thir.params);
    let new_body = self.reduce_body();
    self.reduced_thir.get_mut().set_body(new_body);
  }

  fn reduce_body(&self) -> Option<RExpr<'tcx>> {
    let expr_id = ExprId::from_usize(self.thir.exprs.len() - 1);
    Some(self.reduce_expr(&expr_id))
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
      Scope { region_scope: _, lint_level: _, value } => self.handle_scope(value),
      Box { value } => RExprKind::Box { value: self.reduce_expr(value) },
      If { if_then_scope, cond, then, else_opt } => RExprKind::If {
        if_then_scope: *if_then_scope,
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
      Use { source } => RExprKind::Use { source: self.reduce_expr(source) },
      NeverToAny { source } => RExprKind::NeverToAny { source: self.reduce_expr(source) },
      PointerCoercion { cast, source } => {
        RExprKind::PointerCoercion { cast: *cast, source: self.reduce_expr(source) }
      }
      Loop { body } => RExprKind::Loop { body: self.reduce_expr(body) },
      Let { expr, pat } => RExprKind::Let { expr: self.reduce_expr(expr), pat: pat.clone() },
      Match { scrutinee, scrutinee_hir_id, arms, match_source } => RExprKind::Match {
        scrutinee: self.reduce_expr(scrutinee),
        scrutinee_hir_id: *scrutinee_hir_id,
        arms: arms.clone(),
        match_source: *match_source,
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
      AddressOf { mutability, arg } => {
        RExprKind::AddressOf { mutability: *mutability, arg: self.reduce_expr(arg) }
      }
      Break { label, value } => RExprKind::Break { label: *label, value: unwrap_option(value) },
      Continue { label } => RExprKind::Continue { label: *label },
      Return { value } => RExprKind::Return { value: unwrap_option(value) },
      Become { value } => RExprKind::Become { value: self.reduce_expr(value) },
      ConstBlock { did, args } => RExprKind::ConstBlock { did: *did, args: *args },
      Repeat { value, count } => {
        RExprKind::Repeat { value: self.reduce_expr(value), count: *count }
      }
      Array { fields } => {
        RExprKind::Array { fields: fields.iter().map(|f| self.reduce_expr(f)).collect() }
      }
      Tuple { fields } => {
        RExprKind::Tuple { fields: fields.iter().map(|f| self.reduce_expr(f)).collect() }
      }
      Adt(adt_expr) => RExprKind::Adt(adt_expr.clone()),
      PlaceTypeAscription { source, user_ty } => RExprKind::PlaceTypeAscription {
        source: self.reduce_expr(source),
        user_ty: user_ty.clone(),
      },
      ValueTypeAscription { source, user_ty } => RExprKind::ValueTypeAscription {
        source: self.reduce_expr(source),
        user_ty: user_ty.clone(),
      },
      Closure(closure_expr) => RExprKind::Closure(closure_expr.clone()),
      Literal { lit, neg } => RExprKind::Literal { lit: *lit, neg: *neg },
      NonHirLiteral { lit, user_ty } => {
        RExprKind::NonHirLiteral { lit: *lit, user_ty: user_ty.clone() }
      }
      ZstLiteral { user_ty } => RExprKind::ZstLiteral { user_ty: user_ty.clone() },
      NamedConst { def_id, args, user_ty } => {
        RExprKind::NamedConst { def_id: *def_id, args: *args, user_ty: user_ty.clone() }
      }
      ConstParam { param, def_id } => RExprKind::ConstParam { param: *param, def_id: *def_id },
      StaticRef { alloc_id, ty, def_id } => {
        RExprKind::StaticRef { alloc_id: *alloc_id, ty: *ty, def_id: *def_id }
      }
      InlineAsm(expr) => RExprKind::InlineAsm(expr.clone()),
      OffsetOf { container, fields } => {
        RExprKind::OffsetOf { container: *container, fields: *fields }
      }
      ThreadLocalRef(def_id) => RExprKind::ThreadLocalRef(*def_id),
      Yield { value } => RExprKind::Yield { value: self.reduce_expr(value) },
    }
  }
  fn reduce_expr(&self, expr_id: &ExprId) -> RExpr<'tcx> {
    let expr = &self.thir[*expr_id];
    let rexprkind = self.reduce_expr_kind(&expr.kind);
    RExpr::new(rexprkind, expr.span)
  }

  fn handle_scope(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    let scope = &self.thir[*expr_id];
    self.reduce_expr_kind(&scope.kind)
  }

  fn handle_pattern(&self, pat: &Box<Pat<'tcx>>) -> RPat<'tcx> {
    let Pat { ty: _, span, kind } = &**pat;
    RPat { kind: kind.clone(), span: *span }
  }

  fn handle_stmt(&self, stmt_id: StmtId) -> RStmt<'tcx> {
    let Stmt { kind } = &self.thir.stmts[stmt_id];
    match kind {
      StmtKind::Expr { scope: _, expr } => {
        RStmt { kind: RStmtKind::Expr { expr: self.reduce_expr(expr) } }
      }
      StmtKind::Let {
        remainder_scope: _,
        init_scope: _,
        pattern,
        initializer,
        else_block,
        lint_level: _,
        span,
      } => RStmt {
        kind: RStmtKind::Let {
          pattern: Box::new(self.handle_pattern(pattern)),
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
}

// R: Reduced
#[derive(Debug)]
pub struct RThir<'tcx> {
  params: IndexVec<ParamId, Param<'tcx>>,
  body: Option<RExpr<'tcx>>,
}

impl<'tcx> RThir<'tcx> {
  pub fn new() -> Self { Self { params: IndexVec::new(), body: None } }

  pub fn set_params(&mut self, new_params: &IndexVec<ParamId, Param<'tcx>>) {
    self.params = new_params.clone();
  }

  pub fn set_body(&mut self, new_body: Option<RExpr<'tcx>>) { self.body = new_body; }
}
