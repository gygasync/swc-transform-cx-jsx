use std::ptr::null;

use swc_common::{DUMMY_SP, util::take::Take};
use swc_core::{ast::*, common::{EqIgnoreSpan}};
use swc_core::{
    ast::Program,
    ast::Ident,
    plugin::{plugin_transform, proxies::TransformPluginProgramMetadata},
    testing_transform::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
};
use swc_core::visit::Fold;

pub struct TransformVisitor;

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    let program = program.fold_with(&mut transform_cx());
    program
}

struct CxImports {

}

pub fn transform_cx() -> impl Fold {
    let mut folder = CxImports {};

    folder
}

impl Fold for CxImports {
    fn fold_expr_stmt(&mut self, st: ExprStmt) -> ExprStmt  {
        let expr = st.expr.clone();
        match *expr {
            Expr::JSXElement(jsx_el)=> {
                match jsx_el.opening.name {
                    JSXElementName::Ident(Ident {sym, ..}) => {
                        if sym.to_string() == "cx" {
                            let null_ident = Ident {span: DUMMY_SP, sym: Default::default(), optional : false};
                            return ExprStmt {span: DUMMY_SP, expr: Box::new(Expr::Lit(Lit::Null(Null {span: DUMMY_SP})))};
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        return st;
    }
}


test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {jsx: true, ..Default::default()}),
    |_| transform_cx(),
    doesnt_touch_unwrapped_code,
    r#"<div id="123" />"#,
    r#"<div id="123" />"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {jsx: true, ..Default::default()}),
    |_| transform_cx(),
    converts_empty_cx_tags_to_null,
    r#"<cx></cx>"#,
    r#"null"#
);