// rustc crates
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyKind;
use rustc_span::Span;

// std crates
use std::rc::Rc;

// Own crates
use crate::analyze::helper_struct::*;
use crate::thir::rthir::*;

#[derive(Clone)]
pub struct Lir<'tcx> {
    pub kind: LirKind<'tcx>,
    pub expr: Rc<RExpr<'tcx>>,
}

impl<'tcx> Lir<'tcx> {
    pub fn new(
        ty: TyKind<'tcx>, var_expr: Vec<String>, expr: Rc<RExpr<'tcx>>,
    ) -> Result<Self, AnalysisError> {
        let kind = match ty {
            TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) => {
                LirKind::new(ty, var_expr[0].clone())
            }
            TyKind::Ref(_, ty, _) => LirKind::new_aggregate(ty, var_expr),
            _ => return Err(AnalysisError::UnsupportedPattern(format!("Unknown TyKind: {ty:?}"))),
        };

        Ok(Self { kind, expr })
    }

    pub fn get_span(&self) -> Span { self.expr.span }

    pub fn to_smt(&self) -> &String { self.kind.get_var_expr() }

    pub fn get_var_expr(&self) -> &String { self.kind.get_var_expr() }

    pub fn set_var_expr(&mut self, constraint: String) { self.kind.set_var_expr(constraint) }

    pub fn get_var_expr_by_index(&self, indices: Vec<usize>) -> &String {
        self.kind.get_var_expr_by_index(indices)
    }

    pub fn adapt_var_expr(&mut self, operation: &String, arg: &String, expr: Rc<RExpr<'tcx>>) {
        self.kind.adapt_var_expr(operation, arg);
        self.expr = expr;
    }

    pub fn get_ty(&self) -> TyKind<'tcx> { self.kind.get_ty() }
}

#[derive(Clone)]
pub enum LirKind<'tcx> {
    VarExpr { var_expr: String, ty: TyKind<'tcx> },
    Aggregate { _ty: Ty<'tcx>, fields: Vec<LirKind<'tcx>> },
}

impl<'tcx> LirKind<'tcx> {
    pub fn new(ty: TyKind<'tcx>, var_expr: String) -> Self { LirKind::VarExpr { var_expr, ty } }

    pub fn new_aggregate(ty: Ty<'tcx>, args: Vec<String>) -> Self {
        LirKind::Aggregate {
            _ty: ty,
            fields: args.iter().map(|arg| LirKind::new(*ty.kind(), arg.to_string())).collect(),
        }
    }

    pub fn get_ty(&self) -> TyKind<'tcx> {
        match self {
            LirKind::VarExpr { ty, .. } => ty.clone(),
            LirKind::Aggregate { fields, .. } => fields[0].get_ty(),
        }
    }

    pub fn get_var_expr(&self) -> &String {
        match self {
            LirKind::VarExpr { var_expr, .. } => var_expr,
            LirKind::Aggregate { fields, .. } => fields[0].get_var_expr(),
        }
    }

    pub fn get_var_expr_by_index(&self, mut indices: Vec<usize>) -> &String {
        match self {
            LirKind::Aggregate { fields, .. } => {
                fields[indices.remove(0)].get_var_expr_by_index(indices)
            }
            LirKind::VarExpr { var_expr, .. } => var_expr,
        }
    }

    pub fn set_var_expr(&mut self, new_var_expr: String) {
        match self {
            LirKind::VarExpr { var_expr, .. } => *var_expr = new_var_expr,
            LirKind::Aggregate { fields, .. } => fields[0].set_var_expr(new_var_expr),
        }
    }

    pub fn set_var_expr_by_index(&mut self, new_var_expr: String, mut indices: Vec<usize>) {
        match self {
            LirKind::VarExpr { var_expr, .. } => *var_expr = new_var_expr,
            LirKind::Aggregate { fields, .. } => {
                fields[indices.remove(0)].set_var_expr_by_index(new_var_expr, indices)
            }
        }
    }

    pub fn adapt_var_expr(&mut self, operation: &String, arg: &String) {
        self.set_var_expr(format!("({} {} {})", operation, self.get_var_expr(), arg));
    }
}
