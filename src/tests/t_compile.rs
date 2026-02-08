use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::context::ProgramParsedContext;
use crate::frontend::program::flatten_program_module;
use crate::frontend::{
    FrontendError, ModuleLoader, ModulePath, discover_and_parse_program_with_loader,
};
use crate::tree::parsed::{Expr, ExprKind};
use crate::tree::visit::{self, Visitor};

struct MockLoader {
    modules: HashMap<String, String>,
}

impl ModuleLoader for MockLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
        let key = path.to_string();
        if let Some(src) = self.modules.get(&key) {
            Ok((PathBuf::from(format!("{key}.mc")), src.clone()))
        } else {
            Err(FrontendError::UnknownModule(path.clone()))
        }
    }
}

#[derive(Default)]
struct CallRewriteStats {
    saw_rewritten_call: bool,
    saw_alias_method_call: bool,
}

impl Visitor<()> for CallRewriteStats {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Call { callee, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "answer"
        {
            self.saw_rewritten_call = true;
        }
        if let ExprKind::MethodCall { callee, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "util"
        {
            self.saw_alias_method_call = true;
        }
        visit::walk_expr(self, expr);
    }
}

#[test]
fn flatten_program_rewrites_alias_method_call_to_plain_call() {
    let entry_src = r#"
        requires {
            app.util
        }

        fn main() -> u64 {
            util.answer()
        }
    "#;
    let mut modules = HashMap::new();
    modules.insert(
        "app.util".to_string(),
        "fn answer() -> u64 { 7 }".to_string(),
    );
    let loader = MockLoader { modules };
    let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

    let program = discover_and_parse_program_with_loader(
        entry_src,
        Path::new("app/main.mc"),
        entry_path,
        &loader,
    )
    .expect("program should parse");

    let flattened = flatten_program_module(&ProgramParsedContext::new(program));
    let mut stats = CallRewriteStats::default();
    stats.visit_module(&flattened);

    assert!(stats.saw_rewritten_call);
    assert!(!stats.saw_alias_method_call);
}
