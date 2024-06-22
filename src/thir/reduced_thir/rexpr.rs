use rustc_hir as hir;
use rustc_hir::def_id::DefId;
use rustc_hir::{HirId, MatchSource};
use rustc_middle::middle::region;
use rustc_middle::mir::interpret::AllocId;
use rustc_middle::mir::{BinOp, BorrowKind, UnOp};
use rustc_middle::thir::*;
use rustc_middle::ty::adjustment::PointerCoercion;
use rustc_middle::ty::{
  self, CanonicalUserType, CanonicalUserTypeAnnotation, FnSig, GenericArgsRef, List, Ty, TyCtxt,
  UpvarArgs,
};
use rustc_span::Span;
use rustc_target::abi::*;

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
  /// A `box <value>` expression.
  Box {
    value: RExpr<'tcx>,
  },
  /// An `if` expression.
  If {
    if_then_scope: region::Scope,
    cond: RExpr<'tcx>,
    then: RExpr<'tcx>,
    else_opt: Option<RExpr<'tcx>>,
  },
  /// A function call. Method calls and overloaded operators are converted to plain function calls.
  Call {
    /// The type of the function. This is often a [`FnDef`] or a [`FnPtr`].
    ///
    /// [`FnDef`]: ty::TyKind::FnDef
    /// [`FnPtr`]: ty::TyKind::FnPtr
    ty: Ty<'tcx>,
    /// The function itself.
    fun: RExpr<'tcx>,
    /// The arguments passed to the function.
    ///
    /// Note: in some cases (like calling a closure), the function call `f(...args)` gets
    /// rewritten as a call to a function trait method (e.g. `FnOnce::call_once(f, (...args))`).
    args: Box<[RExpr<'tcx>]>,
    /// Whether this is from an overloaded operator rather than a
    /// function call from HIR. `true` for overloaded function call.
    from_hir_call: bool,
    /// The span of the function, without the dot and receiver
    /// (e.g. `foo(a, b)` in `x.foo(a, b)`).
    fn_span: Span,
  },
  /// A *non-overloaded* dereference.
  Deref {
    arg: RExpr<'tcx>,
  },
  /// A *non-overloaded* binary operation.
  Binary {
    op: BinOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  /// A logical operation. This is distinct from `BinaryOp` because
  /// the operands need to be lazily evaluated.
  LogicalOp {
    op: LogicalOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  /// A *non-overloaded* unary operation. Note that here the deref (`*`)
  /// operator is represented by `ExprKind::Deref`.
  Unary {
    op: UnOp,
    arg: RExpr<'tcx>,
  },
  /// A cast: `<source> as <type>`. The type we cast to is the type of
  /// the parent expression.
  Cast {
    source: RExpr<'tcx>,
  },
  /// Forces its contents to be treated as a value expression, not a place
  /// expression. This is inserted in some places where an operation would
  /// otherwise be erased completely (e.g. some no-op casts), but we still
  /// need to ensure that its operand is treated as a value and not a place.
  Use {
    source: RExpr<'tcx>,
  },
  /// A coercion from `!` to any type.
  NeverToAny {
    source: RExpr<'tcx>,
  },
  /// A pointer coercion. More information can be found in [`PointerCoercion`].
  /// Pointer casts that cannot be done by coercions are represented by [`ExprKind::Cast`].
  PointerCoercion {
    cast: PointerCoercion,
    source: RExpr<'tcx>,
  },
  /// A `loop` expression.
  Loop {
    body: RExpr<'tcx>,
  },
  /// Special expression representing the `let` part of an `if let` or similar construct
  /// (including `if let` guards in match arms, and let-chains formed by `&&`).
  ///
  /// This isn't considered a real expression in surface Rust syntax, so it can
  /// only appear in specific situations, such as within the condition of an `if`.
  ///
  /// (Not to be confused with [`StmtKind::Let`], which is a normal `let` statement.)
  Let {
    expr: RExpr<'tcx>,
    pat: Box<Pat<'tcx>>,
  },
  /// A `match` expression.
  Match {
    scrutinee: RExpr<'tcx>,
    scrutinee_hir_id: HirId,
    arms: Box<[ArmId]>,
    match_source: MatchSource,
  },
  /// A block.
  Block {
    block: BlockId,
  },
  /// An assignment: `lhs = rhs`.
  Assign {
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  /// A *non-overloaded* operation assignment, e.g. `lhs += rhs`.
  AssignOp {
    op: BinOp,
    lhs: RExpr<'tcx>,
    rhs: RExpr<'tcx>,
  },
  /// Access to a field of a struct, a tuple, an union, or an enum.
  Field {
    lhs: RExpr<'tcx>,
    /// Variant containing the field.
    variant_index: VariantIdx,
    /// This can be a named (`.foo`) or unnamed (`.0`) field.
    name: FieldIdx,
  },
  /// A *non-overloaded* indexing operation.
  Index {
    lhs: RExpr<'tcx>,
    index: RExpr<'tcx>,
  },
  /// A local variable.
  VarRef {
    id: LocalVarId,
  },
  /// Used to represent upvars mentioned in a closure/coroutine
  UpvarRef {
    /// DefId of the closure/coroutine
    closure_def_id: DefId,

    /// HirId of the root variable
    var_hir_id: LocalVarId,
  },
  /// A borrow, e.g. `&arg`.
  Borrow {
    borrow_kind: BorrowKind,
    arg: RExpr<'tcx>,
  },
  /// A `&raw [const|mut] $place_expr` raw borrow resulting in type `*[const|mut] T`.
  AddressOf {
    mutability: hir::Mutability,
    arg: RExpr<'tcx>,
  },
  /// A `break` expression.
  Break {
    label: region::Scope,
    value: Option<RExpr<'tcx>>,
  },
  /// A `continue` expression.
  Continue {
    label: region::Scope,
  },
  /// A `return` expression.
  Return {
    value: Option<RExpr<'tcx>>,
  },
  /// A `become` expression.
  Become {
    value: RExpr<'tcx>,
  },
  /// An inline `const` block, e.g. `const {}`.
  ConstBlock {
    did: DefId,
    args: GenericArgsRef<'tcx>,
  },
  /// An array literal constructed from one repeated element, e.g. `[1; 5]`.
  Repeat {
    value: RExpr<'tcx>,
    count: ty::Const<'tcx>,
  },
  /// An array, e.g. `[a, b, c, d]`.
  Array {
    fields: Box<[RExpr<'tcx>]>,
  },
  /// A tuple, e.g. `(a, b, c, d)`.
  Tuple {
    fields: Box<[RExpr<'tcx>]>,
  },
  /// An ADT constructor, e.g. `Foo {x: 1, y: 2}`.
  Adt(Box<AdtExpr<'tcx>>),
  /// A type ascription on a place.
  PlaceTypeAscription {
    source: RExpr<'tcx>,
    /// Type that the user gave to this expression
    user_ty: UserTy<'tcx>,
  },
  /// A type ascription on a value, e.g. `42: i32`.
  ValueTypeAscription {
    source: RExpr<'tcx>,
    /// Type that the user gave to this expression
    user_ty: UserTy<'tcx>,
  },
  /// A closure definition.
  Closure(Box<ClosureExpr<'tcx>>),
  /// A literal.
  Literal {
    lit: &'tcx hir::Lit,
    neg: bool,
  },
  /// For literals that don't correspond to anything in the HIR
  NonHirLiteral {
    lit: ty::ScalarInt,
    user_ty: UserTy<'tcx>,
  },
  /// A literal of a ZST type.
  ZstLiteral {
    user_ty: UserTy<'tcx>,
  },
  /// Associated constants and named constants
  NamedConst {
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
    user_ty: UserTy<'tcx>,
  },
  ConstParam {
    param: ty::ParamConst,
    def_id: DefId,
  },
  // FIXME improve docs for `StaticRef` by distinguishing it from `NamedConst`
  /// A literal containing the address of a `static`.
  ///
  /// This is only distinguished from `Literal` so that we can register some
  /// info for diagnostics.
  StaticRef {
    alloc_id: AllocId,
    ty: Ty<'tcx>,
    def_id: DefId,
  },
  /// An expression taking a reference to a thread local.
  ThreadLocalRef(DefId),
  /// A `yield` expression.
  Yield {
    value: RExpr<'tcx>,
  },
}

impl<'tcx> RExprKind<'tcx> {
  pub fn new(value: Option<RExpr<'tcx>>) -> Self { Self::Return { value } }
}
