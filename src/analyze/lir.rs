// rustc crates
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyKind;
use rustc_span::Span;

// std crates
use std::rc::Rc;

// Own crates
use crate::analyze::helper_struct::*;
use crate::thir::rthir::*;

#[derive(Debug, Clone)]
pub struct Lir<'tcx> {
    pub kind: LirKind<'tcx>,
    pub expr: Rc<RExpr<'tcx>>,
}

impl<'tcx> Lir<'tcx> {
    pub fn new(
        ty: TyKind<'tcx>, assume: Vec<String>, expr: Rc<RExpr<'tcx>>,
    ) -> Result<Self, AnalysisError> {
        let kind = match ty {
            TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) => LirKind::new(ty, assume[0].clone()),
            TyKind::Ref(_, ty, _) => LirKind::new_aggregate(ty.clone(), assume),
            _ => return Err(AnalysisError::UnsupportedPattern("Unknown TyKind".into())),
        };

        Ok(Self { kind, expr })
    }

    pub fn get_span(&self) -> Span { self.expr.span }

    pub fn to_smt(&self) -> &String { self.kind.get_assume() }

    pub fn get_assume(&self) -> &String { self.kind.get_assume() }

    pub fn set_assume(&mut self, constraint: String) { self.kind.set_assume(constraint) }

    pub fn get_assume_by_index(&self, index: usize) -> &String {
        self.kind.get_assume_by_index(index)
    }

    pub fn adapt_assume(&mut self, operation: &String, arg: &String, expr: Rc<RExpr<'tcx>>) {
        self.kind.adapt_assume(operation, arg);
        self.expr = expr;
    }

    pub fn get_ty(&self) -> TyKind<'tcx> { self.kind.get_ty() }
}

#[derive(Debug, Clone)]
pub enum LirKind<'tcx> {
    Path { assume: String, ty: TyKind<'tcx> },
    Aggregate { _ty: Ty<'tcx>, fields: Vec<LirKind<'tcx>> },
}

impl<'tcx> LirKind<'tcx> {
    pub fn new(ty: TyKind<'tcx>, assume: String) -> Self { LirKind::Path { assume, ty } }

    pub fn new_aggregate(ty: Ty<'tcx>, args: Vec<String>) -> Self {
        LirKind::Aggregate {
            _ty: ty.clone(),
            fields: args
                .iter()
                .map(|arg| LirKind::new(ty.kind().clone(), arg.to_string()))
                .collect(),
        }
    }

    pub fn get_ty(&self) -> TyKind<'tcx> {
        match self {
            LirKind::Path { ty, .. } => ty.clone(),
            LirKind::Aggregate { fields, .. } => fields[0].get_ty(),
        }
    }

    pub fn get_assume(&self) -> &String {
        match self {
            LirKind::Path { assume, .. } => assume,
            LirKind::Aggregate { fields, .. } => fields[0].get_assume(),
        }
    }

    pub fn set_assume(&mut self, new_assume: String) {
        match self {
            LirKind::Path { assume, .. } => *assume = new_assume,
            LirKind::Aggregate { fields, .. } => fields[0].set_assume(new_assume),
        }
    }

    pub fn get_assume_by_index(&self, index: usize) -> &String {
        match self {
            LirKind::Aggregate { fields, .. } => fields[index].get_assume(),
            _ => panic!("I'm Path!"),
        }
    }

    pub fn adapt_assume(&mut self, operation: &String, arg: &String) {
        self.set_assume(format!("({} {} {})", operation, self.get_assume(), arg));
    }
}
