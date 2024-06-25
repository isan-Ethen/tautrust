// rustc crates
use rustc_errors::ErrorGuaranteed;
use rustc_hir as hir;
use rustc_hir::def_id::DefId;
use rustc_hir::{BindingMode, HirId, MatchSource};
use rustc_middle::middle::region;
use rustc_middle::mir;
use rustc_middle::mir::{BinOp, BorrowKind, UnOp};
use rustc_middle::thir::*;
use rustc_middle::ty::adjustment::PointerCoercion;
use rustc_middle::ty::{self, CanonicalUserType, GenericArgsRef, Ty};
use rustc_span::{Span, Symbol};
use rustc_target::abi::{FieldIdx, VariantIdx};

// std crates
// Own crates

// R: Reduced
#[derive(Debug)]
pub struct RThir<'tcx> {
  pub params: Vec<RParam<'tcx>>,
  pub body: Option<RExpr<'tcx>>,
}

impl<'tcx> RThir<'tcx> {
  pub fn new() -> Self { Self { params: Vec::new(), body: None } }

  pub fn set_params(&mut self, new_params: Vec<RParam<'tcx>>) { self.params = new_params.clone(); }

  pub fn set_body(&mut self, new_body: Option<RExpr<'tcx>>) { self.body = new_body; }
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
  Wild,

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

  Slice {
    prefix: Box<[Box<RPat<'tcx>>]>,
    slice: Option<Box<RPat<'tcx>>>,
    suffix: Box<[Box<RPat<'tcx>>]>,
  },

  Array {
    prefix: Box<[Box<RPat<'tcx>>]>,
    slice: Option<Box<RPat<'tcx>>>,
    suffix: Box<[Box<RPat<'tcx>>]>,
  },

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
    scrutinee_hir_id: HirId,
    arms: Vec<RArm<'tcx>>,
    match_source: MatchSource,
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
  Closure(Box<ClosureExpr<'tcx>>),
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
