// rustc crates
use rustc_hir as hir;
use rustc_hir::def_id::DefId;
use rustc_hir::BindingMode;
use rustc_middle::middle::region;
use rustc_middle::mir::{
    BinOp, //BorrowKind,
    UnOp,
};
use rustc_middle::thir::*;
use rustc_middle::ty::{CanonicalUserType, Ty};
use rustc_span::{Span, Symbol};
use rustc_target::abi::{FieldIdx, VariantIdx};

// std crates
use std::fmt;
use std::rc::Rc;

// Own crates

// R: Reduced
pub struct RThir<'tcx> {
    pub params: Vec<RParam<'tcx>>,
    pub body: Option<Rc<RExpr<'tcx>>>,
}

impl<'tcx> RThir<'tcx> {
    pub fn new() -> Self { Self { params: Vec::new(), body: None } }

    pub fn set_params(&mut self, new_params: Vec<RParam<'tcx>>) { self.params = new_params; }

    pub fn set_body(&mut self, new_body: Option<Rc<RExpr<'tcx>>>) { self.body = new_body; }
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

    fn format(&mut self) {
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
            self.format_expr(pat, depth_lvl + 2);
            self.add_indented_string(")", depth_lvl + 1);
        } else {
            self.add_indented_string("param: None", depth_lvl + 1);
        }

        self.add_indented_string("}", depth_lvl);
    }

    fn format_pat_kind(&mut self, pat_kind: &RPatKind<'tcx>, depth_lvl: usize) {
        self.add_indented_string("PatKind {", depth_lvl);

        match pat_kind {
            RPatKind::Wild => {
                self.add_indented_string("Wild", depth_lvl + 1);
            }
            RPatKind::Binding { name, mode, var, ty, subpattern, is_primary } => {
                self.add_indented_string("Binding {", depth_lvl + 1);
                self.add_indented_string(&format!("name: {name:?}"), depth_lvl + 2);
                self.add_indented_string(&format!("mode: {mode:?}"), depth_lvl + 2);
                self.add_indented_string(&format!("var: {var:?}"), depth_lvl + 2);
                self.add_indented_string(&format!("ty: {ty:?}"), depth_lvl + 2);
                self.add_indented_string(&format!("is_primary: {is_primary:?}"), depth_lvl + 2);

                if let Some(subpattern) = subpattern {
                    self.add_indented_string("subpattern: Some( ", depth_lvl + 2);
                    self.format_expr(subpattern, depth_lvl + 3);
                    self.add_indented_string(")", depth_lvl + 2);
                } else {
                    self.add_indented_string("subpattern: None", depth_lvl + 2);
                }

                self.add_indented_string("}", depth_lvl + 1);
            }
            RPatKind::Deref { subpattern } => {
                self.add_indented_string("Deref { ", depth_lvl + 1);
                self.add_indented_string("subpattern:", depth_lvl + 2);
                self.format_expr(subpattern, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl + 1);
            }
            RPatKind::DerefPattern { subpattern, mutability } => {
                self.add_indented_string("DerefPattern { ", depth_lvl + 1);
                self.add_indented_string(&format!("mutability: {mutability:?}"), depth_lvl + 2);
                self.add_indented_string("subpattern:", depth_lvl + 2);
                self.format_expr(subpattern, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl + 1);
            }
            RPatKind::Or { pats } => {
                self.add_indented_string("Or {", depth_lvl + 1);
                self.add_indented_string("pats: [", depth_lvl + 2);
                for pat in pats.iter() {
                    self.format_expr(pat, depth_lvl + 3);
                }
                self.add_indented_string("]", depth_lvl + 2);
                self.add_indented_string("}", depth_lvl + 1);
            }
        }

        self.add_indented_string("}", depth_lvl);
    }

    fn format_expr(&mut self, expr: &Rc<RExpr<'tcx>>, depth_lvl: usize) {
        let RExpr { span, kind } = &**expr;
        self.add_indented_string("Expr {", depth_lvl);
        self.add_indented_string(&format!("span: {span:?}"), depth_lvl + 1);
        self.add_indented_string("kind:", depth_lvl + 1);
        self.format_expr_kind(&kind, depth_lvl + 2);
        self.add_indented_string("}", depth_lvl);
    }

    fn format_expr_kind(&mut self, expr_kind: &RExprKind<'tcx>, depth_lvl: usize) {
        use RExprKind::*;

        match expr_kind {
            Pat { kind } => {
                self.add_indented_string("Pat {", depth_lvl);
                self.format_pat_kind(kind, depth_lvl + 1);
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
                self.add_indented_string(&format!("ty: {ty:?}"), depth_lvl + 1);
                self.add_indented_string(&format!("from_hir_call: {from_hir_call}"), depth_lvl + 1);
                self.add_indented_string(&format!("fn_span: {fn_span:?}"), depth_lvl + 1);
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
                self.add_indented_string(&format!("op: {op:?}"), depth_lvl + 1);
                self.add_indented_string("lhs:", depth_lvl + 1);
                self.format_expr(lhs, depth_lvl + 2);
                self.add_indented_string("rhs:", depth_lvl + 1);
                self.format_expr(rhs, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl);
            }
            LogicalOp { op, lhs, rhs } => {
                self.add_indented_string("LogicalOp {", depth_lvl);
                self.add_indented_string(&format!("op: {op:?}"), depth_lvl + 1);
                self.add_indented_string("lhs:", depth_lvl + 1);
                self.format_expr(lhs, depth_lvl + 2);
                self.add_indented_string("rhs:", depth_lvl + 1);
                self.format_expr(rhs, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl);
            }
            Unary { op, arg } => {
                self.add_indented_string("Unary {", depth_lvl);
                self.add_indented_string(&format!("op: {op:?}"), depth_lvl + 1);
                self.add_indented_string("arg:", depth_lvl + 1);
                self.format_expr(arg, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl);
            }
            LetBinding { expr, pat } => {
                self.add_indented_string("LetBinding {", depth_lvl);
                self.add_indented_string("expr:", depth_lvl + 1);
                self.format_expr(expr, depth_lvl + 2);
                self.add_indented_string(&format!("pat: {pat:?}"), depth_lvl + 1);
                self.add_indented_string("}", depth_lvl);
            }
            Block { stmts, expr } => {
                self.add_indented_string("Block {", depth_lvl);

                if stmts.len() > 0 {
                    self.add_indented_string("stmts: [", depth_lvl + 1);
                    for stmt in stmts.iter() {
                        self.format_expr(stmt, depth_lvl + 2);
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
                self.add_indented_string(&format!("op: {op:?}"), depth_lvl + 1);
                self.add_indented_string("lhs:", depth_lvl + 1);
                self.format_expr(lhs, depth_lvl + 2);
                self.add_indented_string("rhs:", depth_lvl + 1);
                self.format_expr(rhs, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl);
            }
            Field { lhs, variant_index, name } => {
                self.add_indented_string("Field {", depth_lvl);
                self.add_indented_string(
                    &format!("variant_index: {variant_index:?}"),
                    depth_lvl + 1,
                );
                self.add_indented_string(&format!("name: {name:?}"), depth_lvl + 1);
                self.add_indented_string("lhs:", depth_lvl + 1);
                self.format_expr(lhs, depth_lvl + 2);
                self.add_indented_string("}", depth_lvl);
            }
            VarRef { id } => {
                self.add_indented_string("VarRef {", depth_lvl);
                self.add_indented_string(&format!("id: {id:?}"), depth_lvl + 1);
                self.add_indented_string("}", depth_lvl);
            }
            UpvarRef { closure_def_id, var_hir_id } => {
                self.add_indented_string("UpvarRef {", depth_lvl);
                self.add_indented_string(
                    &format!("closure_def_id: {closure_def_id:?}"),
                    depth_lvl + 1,
                );
                self.add_indented_string(&format!("var_hir_id: {var_hir_id:?}"), depth_lvl + 1);
                self.add_indented_string("}", depth_lvl);
            }
            Borrow {
                //borrow_kind,
                arg,
            } => {
                self.add_indented_string("Borrow (", depth_lvl);
                // self.add_indented_string(&format!("borrow_kind: {:?}", borrow_kind), depth_lvl + 1);
                self.add_indented_string("arg:", depth_lvl + 1);
                self.format_expr(arg, depth_lvl + 2);
                self.add_indented_string(")", depth_lvl);
            }
            Break { label, value } => {
                self.add_indented_string("Break (", depth_lvl);
                self.add_indented_string(&format!("label: {label:?}"), depth_lvl + 1);

                if let Some(value) = value {
                    self.add_indented_string("value:", depth_lvl + 1);
                    self.format_expr(value, depth_lvl + 2);
                }

                self.add_indented_string(")", depth_lvl);
            }
            Return { value } => {
                self.add_indented_string("Return {", depth_lvl);
                self.add_indented_string("value:", depth_lvl + 1);

                if let Some(value) = value {
                    self.format_expr(value, depth_lvl + 2);
                }

                self.add_indented_string("}", depth_lvl);
            }
            Literal { lit, neg } => {
                self.add_indented_string(
                    &format!("Literal( lit: {:?}, neg: {:?})\n", lit, neg),
                    depth_lvl,
                );
            }
            ZstLiteral { user_ty } => {
                self.add_indented_string(&format!("ZstLiteral(user_ty: {user_ty:?})"), depth_lvl);
            }
            LetStmt { pattern, initializer } => {
                self.add_indented_string("LetStmt {", depth_lvl + 1);

                self.add_indented_string("pattern:", depth_lvl + 2);
                self.format_expr(pattern, depth_lvl + 3);
                self.add_indented_string(",", depth_lvl + 2);

                if let Some(init) = initializer {
                    self.add_indented_string("initializer: Some(", depth_lvl + 2);
                    self.format_expr(init, depth_lvl + 3);
                    self.add_indented_string(")", depth_lvl + 2);
                } else {
                    self.add_indented_string("initializer: None", depth_lvl + 2);
                }
                self.add_indented_string("}", depth_lvl + 1);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct RParam<'tcx> {
    pub pat: Option<Rc<RExpr<'tcx>>>,
}

impl<'tcx> RParam<'tcx> {
    pub fn new(pat: Option<Rc<RExpr<'tcx>>>) -> Self { Self { pat } }
}

#[derive(Clone, Debug)]
pub enum RPatKind<'tcx> {
    Wild,

    Binding {
        name: Symbol,
        mode: BindingMode,
        var: LocalVarId,
        ty: Ty<'tcx>,
        subpattern: Option<Rc<RExpr<'tcx>>>,
        is_primary: bool,
    },

    Deref {
        subpattern: Rc<RExpr<'tcx>>,
    },

    DerefPattern {
        subpattern: Rc<RExpr<'tcx>>,
        mutability: hir::Mutability,
    },

    Or {
        pats: Box<[Rc<RExpr<'tcx>>]>,
    },
}

#[derive(Clone, Debug)]
pub struct RExpr<'tcx> {
    pub kind: RExprKind<'tcx>,
    pub span: Span,
}

impl<'tcx> RExpr<'tcx> {
    pub fn new(kind: RExprKind<'tcx>, span: Span) -> Self { Self { kind, span } }
}

type UserTy<'tcx> = Option<Box<CanonicalUserType<'tcx>>>;

#[derive(Clone, Debug)]
pub enum RExprKind<'tcx> {
    If {
        cond: Rc<RExpr<'tcx>>,
        then: Rc<RExpr<'tcx>>,
        else_opt: Option<Rc<RExpr<'tcx>>>,
    },
    Call {
        ty: Ty<'tcx>,
        fun: Rc<RExpr<'tcx>>,
        args: Box<[Rc<RExpr<'tcx>>]>,
        from_hir_call: bool,
        fn_span: Span,
    },
    Deref {
        arg: Rc<RExpr<'tcx>>,
    },
    Binary {
        op: BinOp,
        lhs: Rc<RExpr<'tcx>>,
        rhs: Rc<RExpr<'tcx>>,
    },
    LogicalOp {
        op: LogicalOp,
        lhs: Rc<RExpr<'tcx>>,
        rhs: Rc<RExpr<'tcx>>,
    },
    Unary {
        op: UnOp,
        arg: Rc<RExpr<'tcx>>,
    },
    LetBinding {
        expr: Rc<RExpr<'tcx>>,
        pat: Rc<RExpr<'tcx>>,
    },
    Pat {
        kind: RPatKind<'tcx>,
    },
    Block {
        stmts: Vec<Rc<RExpr<'tcx>>>,
        expr: Option<Rc<RExpr<'tcx>>>,
    },
    Assign {
        lhs: Rc<RExpr<'tcx>>,
        rhs: Rc<RExpr<'tcx>>,
    },
    AssignOp {
        op: BinOp,
        lhs: Rc<RExpr<'tcx>>,
        rhs: Rc<RExpr<'tcx>>,
    },
    Field {
        lhs: Rc<RExpr<'tcx>>,
        variant_index: VariantIdx,
        name: FieldIdx,
    },
    VarRef {
        id: LocalVarId,
    },
    UpvarRef {
        closure_def_id: DefId,
        var_hir_id: LocalVarId,
    },
    Borrow {
        // borrow_kind: BorrowKind,
        arg: Rc<RExpr<'tcx>>,
    },
    Break {
        label: region::Scope,
        value: Option<Rc<RExpr<'tcx>>>,
    },
    Return {
        value: Option<Rc<RExpr<'tcx>>>,
    },
    Literal {
        lit: &'tcx hir::Lit,
        neg: bool,
    },
    ZstLiteral {
        user_ty: UserTy<'tcx>,
    },
    LetStmt {
        pattern: Rc<RExpr<'tcx>>,
        initializer: Option<Rc<RExpr<'tcx>>>,
    },
}
