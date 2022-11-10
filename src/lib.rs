use std::path::PathBuf;

use swc_core::common::{Mark, DUMMY_SP};
use swc_core::ecma::ast::{Expr, JSXElementChild, JSXElementName, JSXText};
use swc_core::ecma::atoms::Atom;
use swc_core::ecma::transforms::base::resolver;
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};
use swc_core::{
    common::chain,
    ecma::{
        ast::Program,
        transforms::testing::{test, test_fixture},
        visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
    },
};
use swc_ecma_parser::{EsConfig, Syntax};

pub struct TransformVisitor;

impl VisitMut for TransformVisitor {
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        // match expr.borrow() {
        //     Expr::JSXElement(jsx_el) => {

        //     }
        //     _ => {}
        // }

        if let Expr::JSXElement(jsx_el) = expr {
            print!("WE HIT CX");
            if let JSXElementName::Ident(ident) = &mut jsx_el.opening.name {
                if ident.sym.to_string() == "cx" {
                    let test = JSXElementChild::JSXText(JSXText {
                        span: DUMMY_SP,
                        value: "TEST".into(),
                        raw: Atom::default(),
                    });
                    jsx_el.children.append(&mut vec![test]);
                }
            }
        }
    }
}

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor))
}

#[testing::fixture("tests/**/input.js")]
fn exec(input: PathBuf) {
    let output = input.with_file_name("output.js");
    test_fixture(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        &|_| {
            chain!(
                resolver(Mark::new(), Mark::new(), true),
                as_folder(TransformVisitor {})
            )
        }, // This works but i do not know how and why
        &input,
        &output,
    )
}
