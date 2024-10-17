// rustc crates
use rustc_middle::thir::*;

// std crates
use std::rc::Rc;

// Own crates
use crate::thir::rthir::*;

pub fn reduce_thir(thir: Thir) -> RThir {
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
        RParam::new(if let Some(pat) = pat { Some(self.reduce_pattern(pat)) } else { None })
    }

    fn reduce_pattern(&self, pat: &Box<Pat<'tcx>>) -> Rc<RExpr<'tcx>> {
        let Pat { span, kind, .. } = &**pat;
        Rc::new(RExpr::new(RExprKind::Pat { kind: self.reduce_pattern_kind(kind) }, *span))
    }

    fn reduce_pattern_kind(&self, pat_kind: &PatKind<'tcx>) -> RPatKind<'tcx> {
        match pat_kind {
            // PatKind::Wild => RPatKind::Wild,
            PatKind::Binding { name, mode, var, ty, subpattern, is_primary } => RPatKind::Binding {
                name: *name,
                mode: *mode,
                var: *var,
                ty: *ty,
                subpattern: if let Some(pat) = subpattern {
                    Some(self.reduce_pattern(pat))
                } else {
                    None
                },
                is_primary: *is_primary,
            },
            // PatKind::Deref { subpattern } => {
            //     RPatKind::Deref { subpattern: self.reduce_pattern(subpattern) }
            // }
            // PatKind::DerefPattern { subpattern, mutability } => RPatKind::DerefPattern {
            //     subpattern: self.reduce_pattern(subpattern),
            //     mutability: *mutability,
            // },
            // PatKind::Or { pats } => RPatKind::Or {
            //     pats: pats
            //         .iter()
            //         .map(|pat| self.reduce_pattern(pat))
            //         .collect::<Vec<Rc<RExpr<'tcx>>>>()
            //         .into_boxed_slice(),
            // },
            _ => unimplemented!(),
        }
    }

    fn reduce_body(&self) -> Option<Rc<RExpr<'tcx>>> {
        use rustc_middle::thir::ExprKind::*;
        let expr_id = ExprId::from_usize(self.thir.exprs.len() - 1);
        if let Scope { value, .. } = &self.thir[expr_id].kind {
            match &self.thir[*value].kind {
                Borrow { arg, .. } => {
                    if let Deref { arg } = &self.thir[*arg].kind {
                        Some(self.reduce_expr(arg))
                    } else {
                        panic!("Unknown if borrow")
                    }
                }
                _ => Some(self.reduce_expr(value)),
            }
        } else {
            panic!("Unknown then pattern")
        }
    }

    fn reduce_expr(&self, expr_id: &ExprId) -> Rc<RExpr<'tcx>> {
        let expr = &self.thir[*expr_id];
        let rexprkind = self.reduce_expr_kind(&expr.kind);
        Rc::new(RExpr::new(rexprkind, expr.span))
    }

    fn reduce_expr_kind(&self, expr_kind: &ExprKind<'tcx>) -> RExprKind<'tcx> {
        use rustc_middle::thir::ExprKind::*;
        // let unwrap_option = |value: &Option<ExprId>| {
        //     if let Some(expr_id) = value {
        //         Some(self.reduce_expr(expr_id))
        //     } else {
        //         None
        //     }
        // };

        match expr_kind {
            Scope { value, .. } => self.handle_scope(value),
            // If { cond, then, else_opt, .. } => RExprKind::If {
            //     cond: self.reduce_expr(cond),
            //     then: if let Scope { value, .. } = &self.thir[*then].kind {
            //         match &self.thir[*value].kind {
            //             Borrow { arg, .. } => {
            //                 if let Deref { arg } = &self.thir[*arg].kind {
            //                     self.reduce_expr(arg)
            //                 } else {
            //                     panic!("Unknown if borrow")
            //                 }
            //             }
            //             _ => self.reduce_expr(value),
            //         }
            //     } else {
            //         panic!("Unknown then pattern")
            //     },
            //     else_opt: if let Some(expr_id) = else_opt {
            //         if let Scope { value, .. } = &self.thir[*expr_id].kind {
            //             match &self.thir[*value].kind {
            //                 Borrow { arg, .. } => {
            //                     if let Deref { arg } = &self.thir[*arg].kind {
            //                         Some(self.reduce_expr(arg))
            //                     } else {
            //                         panic!("Unknown if borrow")
            //                     }
            //                 }
            //                 _ => Some(self.reduce_expr(value)),
            //             }
            //         } else {
            //             panic!("Unknown else_opt pattern")
            //         }
            //     } else {
            //         None
            //     },
            // },
            // Call { ty, fun, args, from_hir_call, fn_span } => RExprKind::Call {
            //     ty: *ty,
            //     fun: self.reduce_expr(fun),
            //     args: args.iter().map(|arg| self.reduce_expr(arg)).collect(),
            //     from_hir_call: *from_hir_call,
            //     fn_span: *fn_span,
            // },
            // Deref { arg } => RExprKind::Deref { arg: self.reduce_expr(arg) },
            Binary { op, lhs, rhs } => RExprKind::Binary {
                op: *op,
                lhs: self.reduce_expr(lhs),
                rhs: self.reduce_expr(rhs),
            },
            // LogicalOp { op, lhs, rhs } => RExprKind::LogicalOp {
            //     op: *op,
            //     lhs: self.reduce_expr(lhs),
            //     rhs: self.reduce_expr(rhs),
            // },
            // Unary { op, arg } => RExprKind::Unary { op: *op, arg: self.reduce_expr(arg) },
            // Use { source } => self.handle_use(source),
            // NeverToAny { source } => self.handle_never_to_any(source),
            // Let { expr, pat } => RExprKind::LetBinding {
            //     expr: self.reduce_expr(expr),
            //     pat: self.reduce_pattern(pat),
            // },
            Block { block } => self.handle_block(block),
            // Assign { lhs, rhs } => {
            //     RExprKind::Assign { lhs: self.reduce_expr(lhs), rhs: self.reduce_expr(rhs) }
            // }
            // AssignOp { op, lhs, rhs } => RExprKind::AssignOp {
            //     op: *op,
            //     lhs: self.reduce_expr(lhs),
            //     rhs: self.reduce_expr(rhs),
            // },
            // Field { lhs, variant_index, name } => RExprKind::Field {
            //     lhs: self.reduce_expr(lhs),
            //     variant_index: *variant_index,
            //     name: *name,
            // },
            VarRef { id } => RExprKind::VarRef { id: *id },
            // UpvarRef { closure_def_id, var_hir_id } => {
            //     RExprKind::UpvarRef { closure_def_id: *closure_def_id, var_hir_id: *var_hir_id }
            // }
            // Borrow { borrow_kind, arg } => {
            //     use rustc_middle::mir::MutBorrowKind;
            //     match borrow_kind {
            //         rustc_middle::mir::BorrowKind::Mut { kind } => match kind {
            //             MutBorrowKind::TwoPhaseBorrow => self.handle_two_phase_borrow(arg),
            //             MutBorrowKind::Default => RExprKind::Borrow { arg: self.reduce_expr(arg) },
            //             _ => panic!("MutBorrowKind::ClosureCpature is not supported"),
            //         },
            //         _ => {
            //             println!("{borrow_kind:?}");
            //             panic!("Other BorrowKinds are not supported!")
            //         }
            //     }
            // }
            // Break { label, value } => {
            //     RExprKind::Break { label: *label, value: unwrap_option(value) }
            // }
            // Return { value } => RExprKind::Return { value: unwrap_option(value) },
            Literal { lit, neg } => RExprKind::Literal { lit: *lit, neg: *neg },
            // ZstLiteral { user_ty } => RExprKind::ZstLiteral { user_ty: user_ty.clone() },
            _ => unimplemented!(),
        }
    }

    fn handle_scope(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
        let scope = &self.thir[*expr_id];
        self.reduce_expr_kind(&scope.kind)
    }

    // fn handle_use(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    //     let use_expr = &self.thir[*expr_id];
    //     self.reduce_expr_kind(&use_expr.kind)
    // }

    // fn handle_never_to_any(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    //     let never_to_any = &self.thir[*expr_id];
    //     self.reduce_expr_kind(&never_to_any.kind)
    // }

    fn handle_block(&self, block_id: &BlockId) -> RExprKind<'tcx> {
        let block = &self.thir.blocks[*block_id];

        let mut stmts = Vec::new();
        for stmt in block.stmts.iter() {
            stmts.push(self.handle_stmt(*stmt));
        }

        RExprKind::Block {
            stmts,
            expr: if let Some(expr_id) = block.expr {
                Some(self.reduce_expr(&expr_id))
            } else {
                None
            },
        }
    }

    // fn handle_two_phase_borrow(&self, expr_id: &ExprId) -> RExprKind<'tcx> {
    //     let expr = &self.thir[*expr_id];
    //     self.reduce_expr_kind(&expr.kind)
    // }

    fn handle_stmt(&self, stmt_id: StmtId) -> Rc<RExpr<'tcx>> {
        let Stmt { kind } = &self.thir.stmts[stmt_id];
        match kind {
            StmtKind::Expr { expr, .. } => self.reduce_expr(expr),
            StmtKind::Let { pattern, initializer, span, .. } => Rc::new(RExpr::new(
                RExprKind::LetStmt {
                    pattern: self.reduce_pattern(pattern),
                    initializer: if let Some(expr_id) = initializer {
                        Some(self.reduce_expr(&expr_id))
                    } else {
                        None
                    },
                },
                *span,
            )),
        }
    }
}
