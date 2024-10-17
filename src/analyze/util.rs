// rustc crates
use rustc_span::{
    def_id::{
        // DefId,
        LocalDefId,
    },
    Span,
};

// std crates
// Own crates
use crate::analyze::*;

impl<'tcx> Analyzer<'tcx> {
    pub fn get_fn(&self, fn_id: LocalDefId) -> Result<Rc<RThir<'tcx>>, AnalysisError> {
        self.fn_map.get(&fn_id).cloned().ok_or(AnalysisError::FunctionNotFound(fn_id))
    }

    // pub fn get_local_fn(&self, def_id: &DefId) -> Option<Rc<RThir<'tcx>>> {
    //     if def_id.is_local() {
    //         Some(self.fn_map.get(&def_id.expect_local()).expect("Get local fn failed").clone())
    //     } else {
    //         None
    //     }
    // }

    // pub fn get_fn_info(&self, def_id: &DefId) -> Vec<String> {
    //     let def_path = self.tcx.def_path_str(*def_id);
    //     def_path
    //         .split(|c| c == ':' || c == '"' || c == '\\')
    //         .filter(|s| !s.is_empty())
    //         .map(String::from)
    //         .collect()
    // }

    pub fn span_to_str(span: &Span) -> String {
        let span_str = format!("{span:?}");
        let parts: Vec<&str> = span_str.split(':').collect();
        let file = parts[0].replace(".rs", "");
        let line = parts[1];
        let column = parts[2].split_whitespace().next().unwrap();

        let sanitized_file = file.replace(|c: char| !c.is_alphanumeric(), "_");

        format!("{sanitized_file}_L{line}_C{column}")
    }

    pub fn expr_to_id(expr: Rc<RExpr<'tcx>>) -> LocalVarId {
        match &expr.kind {
            RExprKind::VarRef { id } => id.clone(),
            // RExprKind::Deref { arg, .. } => {
            //     if let RExprKind::VarRef { id } = &arg.kind {
            //         *id
            //     } else {
            //         panic!()
            //     }
            // }
            _ => {
                eprintln!("{expr:?}");
                unreachable!()
            }
        }
    }
}
