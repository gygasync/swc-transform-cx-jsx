#![feature(drain_filter)]
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

// impl VisitMut for TransformVisitor {
//     // fn visit_mut_jsx_element_children(&mut self, children: &mut Vec<JSXElementChild>) {

//     // }

//     fn visit_mut_jsx_element(&mut self, el: &mut JSXElement) {
//         self.visit_mut_jsx_element_children(&mut el.children);
//         let mut not_orphans: Vec<i32> = vec![];
//         let mut i = 0;
//         for child in &mut el.children {
//             match child {
//                 JSXElementChild::JSXElement(jsx_el) => match &mut jsx_el.opening.name {
//                     JSXElementName::Ident(Ident { sym, ..}) => {
//                         if sym.to_string() == "cx" || sym.to_string() == "Cx" {
//                             println!("orphan test");
//                             if jsx_el.children.len() != 0 {
//                                 println!("orphan");
//                                 not_orphans.push(i);
//                             }
//                         }
//                     },
//                     _=>{}
//                 }
//                 _=>{}
//             }
//             i+=1;
//         }
        

//         // el.children = el.children.take();
//     }
// }

pub struct TransformVisitor;

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    let program = program.fold_with(&mut transform_cx());
    program
    // program.fold_with(&mut as_folder(TransformVisitor))
}

struct CxImports {

}

pub fn transform_cx() -> impl Fold {
    let mut folder = CxImports {};

    folder
}

<<<<<<< HEAD

fn transform_jsx_child_el(el: &JSXElementChild) -> &JSXElementChild {
    match el {
        JSXElementChild::JSXElement(jsx_el) => {
            if (jsx_el.children.len() > 0) {
                let kids: Vec<JSXElementChild> = vec![];
                let new_children: Vec<&JSXElementChild> = jsx_el.children.iter()
                .map(|child| transform_jsx_child_el(child)).collect();
            } else {
                return el;
            }
        },
        _ => {}
    }

    el
}
=======
// const CX_TAGS: &'static [&'static str; 2] = &["cx", "Cx"];
>>>>>>> 8c0c470c30798d10d68291823b9bcf45f9768a89

impl Fold for CxImports {
    fn fold_expr_stmt(&mut self, st: ExprStmt) -> ExprStmt  {
        let empty_element = JSXElement {span: DUMMY_SP, opening: JSXOpeningElement::dummy(), children: vec![], closing: Option::None };
        let mut result: ExprStmt = ExprStmt {span: DUMMY_SP, expr: Box::new(Expr::Lit(Lit::Null(Null {span: DUMMY_SP})))};
        let expr = st.expr.clone();
        match *expr {
            Expr::JSXElement(jsx_el)=> {
<<<<<<< HEAD
                    let element = self.fold_jsx_element(*jsx_el);
                    if element != empty_element {
                        result = ExprStmt {span: DUMMY_SP, expr: Box::new(Expr::JSXElement(Box::from(element)))};
=======
                match jsx_el.opening.name {
                    JSXElementName::Ident(Ident {sym, ..}) => {
                        if sym.to_string() == "cx" || sym.to_string() == "Cx" {
                            let null_ident = Ident {span: DUMMY_SP, sym: Default::default(), optional : false};
                            return ExprStmt {span: DUMMY_SP, expr: Box::new(Expr::Lit(Lit::Null(Null {span: DUMMY_SP})))};
                        }
>>>>>>> 8c0c470c30798d10d68291823b9bcf45f9768a89
                    }
                },
                _ => {}
            }

        return result;
    }

    fn fold_jsx_element(&mut self, el: JSXElement) -> JSXElement {
        let empty_element = JSXElement {span: DUMMY_SP, opening: JSXOpeningElement::dummy(), children: vec![], closing: Option::None };
        let children: Vec<JSXElementChild> = el.children.clone();
        let mut folded_children: Vec<JSXElementChild> = vec![];
        
        let opening = el.opening.clone();
        match opening.name {
            JSXElementName::Ident(Ident {sym, ..}) => {
                println!("{}", sym.to_string());
                // Pre-processing
                for child in children {
                    match child {
                        JSXElementChild::JSXElement(jsx_child_el) => {
                            let fold_result: JSXElement = self.fold_jsx_element(*jsx_child_el);
                            if fold_result != empty_element {
                                folded_children.push(JSXElementChild::JSXElement(Box::from(fold_result)));
                            }
                        } ,
                        _ => folded_children.push(child)
                    }
                }

                // println!("Children {}");

                // Actual processing
                if (sym.to_string() == "cx" || sym.to_string() == "Cx") && folded_children.len() == 0 {
                    println!("EMPTY");
                    return empty_element;
                }

                return JSXElement {span: el.span, opening: el.opening, closing: el.closing, children: folded_children};
            
            },
            _ => {}
        }

        return el;
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

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {jsx: true, ..Default::default()}),
    |_| transform_cx(),
    nested_empty_cx_tags_resolve_to_null,
    r#"<div><cx><cx></cx></cx></div>"#,
    r#"<div></div>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {jsx: true, ..Default::default()}),
    |_| transform_cx(),
    nested_non_empty_cx_tags_resolve_to_null,
    r#"<cx><cx><div /></cx></cx>"#,
    r#"<cx><cx><div /></cx></cx>"#
);