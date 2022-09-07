use std::borrow::Borrow;
use std::ptr::null;

use swc_common::{util::take::Take, DUMMY_SP};
use swc_core::visit::Fold;
use swc_core::{
    ast::Ident,
    ast::Program,
    plugin::{plugin_transform, proxies::TransformPluginProgramMetadata},
    testing_transform::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
};
use swc_core::{ast::*, common::EqIgnoreSpan};

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
    let program = program.fold_with(&mut transform_cx(CxOptions {
        trimWhitespace: true,
    }));
    program
    // program.fold_with(&mut as_folder(TransformVisitor))
}

#[derive(Clone, Copy)]
pub struct CxOptions {
    trimWhitespace: bool,
}

struct CxImports {
    options: CxOptions,
}

pub fn transform_cx(options: CxOptions) -> impl Fold {
    let mut folder = CxImports { options };

    folder
}

fn optimistic_eval_bool_lit(lit: Lit) -> bool {
    match lit {
        Lit::Bool(bool_lit) => {
            return bool_lit.value;
        }
        _ => {
            return true;
        }
    }
}

fn eval_bool_attribute_option(opt: Option<JSXAttrValue>) -> bool {
    match opt {
        Some(attr_value) => match attr_value {
            JSXAttrValue::Lit(lit) => {
                return optimistic_eval_bool_lit(lit);
            }
            JSXAttrValue::JSXExprContainer(jsx_expr) => match jsx_expr.expr {
                JSXExpr::Expr(expr) => match *expr {
                    Expr::Lit(lit) => {
                        return optimistic_eval_bool_lit(lit);
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        },
        None => {}
    }

    return true;
}

impl CxImports {
    fn transform_jsx_child_element(&mut self, element: JSXElementChild) -> JSXElementChild {
        match element {
            JSXElementChild::JSXElement(el) => {
                let transformed_el = self.transform_cx_jsx_element(*el);
                match transformed_el {
                    Expr::JSXElement(jsx_el) => return JSXElementChild::JSXElement(jsx_el),
                    _ => panic!("Unable to process JSX Element Child"),
                }
            }
            _ => panic!("Unable to process JSX element"),
        }
    }

    fn transform_cx_jsx_element(&mut self, el: JSXElement) -> Expr {
        let mut children: Vec<JSXElementChild> = el.children.clone();
        let opening: JSXOpeningElement = el.opening.clone();
        let closing: Option<JSXClosingElement> = el.closing.clone();

        let tag_name = get_jsx_el_tag_name(el);

        if tag_name == "cx" || tag_name == "Cx" {
            let transformed_children = children
                .iter()
                .filter(|&c| match *c {
                    JSXElementChild::JSXElement(..) => true,
                    _ => false,
                })
                .map(|c| self.transform_jsx_child_element(c.clone())) // Revisit all clone invokations, memory and performance improvmenets are hiding here
                .collect::<Vec<_>>();

            let transformed_children_expr = transformed_children
                .iter()
                .map(|c| match c {
                    JSXElementChild::JSXElement(el) => {
                        Option::from(ExprOrSpread::from(Expr::JSXElement(el.clone())))
                    }
                    _ => panic!("Error creating Cx react element"),
                })
                .collect::<Vec<_>>();

            let transformed_children_array_expr = Expr::Array(ArrayLit {
                span: DUMMY_SP,
                elems: transformed_children_expr,
            });

            if tag_name == "Cx" {
                let mut attrs: Vec<JSXAttrOrSpread> = opening.attrs;

                let items_container = JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::from(transformed_children_array_expr)),
                };

                let items_attr = create_jsx_attribute(
                    "items",
                    Option::from(JSXAttrValue::JSXExprContainer(items_container)),
                );
                attrs.push(JSXAttrOrSpread::JSXAttr(items_attr));

                let element = JSXElement {
                    span: DUMMY_SP,
                    opening: JSXOpeningElement {
                        name: opening.name,
                        span: DUMMY_SP,
                        attrs,
                        self_closing: opening.self_closing,
                        type_args: opening.type_args,
                    },
                    children: vec![],
                    closing: closing,
                };

                return Expr::JSXElement(Box::from(element));
            }

            if !transformed_children.is_empty() {
                return transformed_children_array_expr;
            }

            if transformed_children.len() == 1 {
                let only_child = transformed_children[0].clone();
                return match only_child {
                    JSXElementChild::JSXElement(el) => Expr::JSXElement(el),
                    JSXElementChild::JSXFragment(frag) => Expr::JSXFragment(frag),
                    JSXElementChild::JSXText(text) => Expr::Lit(Lit::from(text)),
                    _ => panic!("Failed transforming only child"),
                };
            }
        }

        let dot_index = tag_name.find('.');

        let mut props: Vec<PropOrSpread> = vec![];
        props.push(create_key_value_prop(
            String::from("$type"),
            Box::from(Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props: vec![],
            })),
        ));
        return Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props,
        });
    }
}

fn get_jsx_el_tag_name(element: JSXElement) -> String {
    match element.opening.name {
        JSXElementName::Ident(Ident { sym, .. }) => sym.to_string(),
        _ => String::from(""),
    }
}

fn create_object_expr(properties: Vec<PropOrSpread>) -> Expr {
    return Expr::Object(ObjectLit {
        span: DUMMY_SP,
        props: properties,
    });
}

fn create_key_value_prop(key: String, value: Box<Expr>) -> PropOrSpread {
    PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
        key: PropName::Str(Str::from(key)),
        value,
    })))
}

fn create_jsx_attribute(name: &str, value: Option<JSXAttrValue>) -> JSXAttr {
    let ident = Ident {
        span: DUMMY_SP,
        sym: name.into(),
        optional: false,
    };

    JSXAttr {
        span: DUMMY_SP,
        name: JSXAttrName::Ident(ident),
        value,
    }
}

impl Fold for CxImports {
    fn fold_expr_stmt(&mut self, st: ExprStmt) -> ExprStmt {
        let empty_element = JSXElement {
            span: DUMMY_SP,
            opening: JSXOpeningElement::dummy(),
            children: vec![],
            closing: Option::None,
        };
        let mut result: ExprStmt = ExprStmt {
            span: DUMMY_SP,
            expr: Box::new(Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))),
        };
        let expr = st.expr.clone();
        match *expr {
            Expr::JSXElement(jsx_el) => {
                let element = self.fold_jsx_element(*jsx_el);
                if element != empty_element {
                    result = ExprStmt {
                        span: DUMMY_SP,
                        expr: Box::new(Expr::JSXElement(Box::from(element))),
                    };
                }
            }
            _ => {}
        }

        return result;
    }

    fn fold_jsx_text(&mut self, mut text: JSXText) -> JSXText {
        if self.options.trimWhitespace {
            text.value = text.value.trim().into();
            text.raw = text.raw.trim().into();
        }

        return text;
    }

    fn fold_jsx_element(&mut self, el: JSXElement) -> JSXElement {
        let local_options = self.options.clone();
        let empty_element = JSXElement {
            span: DUMMY_SP,
            opening: JSXOpeningElement::dummy(),
            children: vec![],
            closing: Option::None,
        };
        let children: Vec<JSXElementChild> = el.children.clone();
        let mut folded_children: Vec<JSXElementChild> = vec![];

        let opening: JSXOpeningElement = el.opening.clone();
        let attrs: Vec<JSXAttrOrSpread> = el.opening.attrs.clone();

        // Examine attributes
        for attr in attrs {
            match attr {
                JSXAttrOrSpread::SpreadElement(spread_el) => match *spread_el.expr {
                    Expr::Ident(Ident { sym, .. }) => {}
                    _ => {}
                },
                JSXAttrOrSpread::JSXAttr(jsx_attr) => match jsx_attr.name {
                    JSXAttrName::Ident(Ident { sym, .. }) => {
                        let attribute = sym.to_string();
                        if attribute == "ws" || attribute == "preserveWhitespace" {
                            let should_preserve_whitespace =
                                eval_bool_attribute_option(jsx_attr.value);
                            self.options.trimWhitespace = !should_preserve_whitespace;
                        }
                    }
                    _ => {}
                },
            }
        }

        match opening.name {
            JSXElementName::Ident(Ident { sym, .. }) => {
                println!("{}", sym.to_string());
                // Processing children
                for child in children {
                    match child {
                        JSXElementChild::JSXElement(jsx_child_el) => {
                            let fold_result: JSXElement = self.fold_jsx_element(*jsx_child_el);
                            if fold_result != empty_element {
                                folded_children
                                    .push(JSXElementChild::JSXElement(Box::from(fold_result)));
                            }
                        }
                        JSXElementChild::JSXText(el) => {
                            let fold_result: JSXText = self.fold_jsx_text(el);
                            folded_children.push(JSXElementChild::JSXText(fold_result));
                        }
                        _ => folded_children.push(child),
                    }
                }
                // Restore options
                self.options = local_options;

                if (sym.to_string() == "cx" || sym.to_string() == "Cx")
                    && folded_children.len() == 0
                {
                    return empty_element;
                }

                return JSXElement {
                    span: el.span,
                    opening: el.opening,
                    closing: el.closing,
                    children: folded_children,
                };
            }
            _ => {}
        }

        return el;
    }
}

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    doesnt_touch_unwrapped_code,
    r#"<div id="123" />"#,
    r#"<div id="123" />"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    converts_empty_cx_tags_to_null,
    r#"<cx></cx>"#,
    r#"null"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    cx_react_tag_should_remain,
    r#"<Cx></Cx>"#,
    r#"<Cx></Cx>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    nested_empty_cx_tags_resolve_to_null,
    r#"<div><cx><cx></cx></cx></div>"#,
    r#"<div></div>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    nested_non_empty_cx_tags_resolve_to_null,
    r#"<cx><cx><div /></cx></cx>"#,
    r#"<cx><cx><div /></cx></cx>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    trims_whitespace_when_flag_is_set,
    r#"<cx><Container><Container /> <div>   <cx></cx> <cx>    <div /></cx></div>   </Container></cx>"#,
    r#"<cx><Container><Container /><div><cx><div/></cx></div></Container></cx>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: false
    }),
    leaves_whitespace_when_flag_is_unset,
    r#"<cx><Container><Container />    </Container></cx>"#,
    r#"<cx><Container><Container />    </Container></cx>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    ws_flag_preserves_whitespace_for_children,
    r#"<cx><Container ws>    <div>    </div>   </Container><Other>    </Other></cx>"#,
    r#"<cx><Container ws>    <div>    </div>   </Container><Other></Other></cx>"#
);

test!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
        jsx: true,
        ..Default::default()
    }),
    |_| transform_cx(CxOptions {
        trimWhitespace: true
    }),
    ws_flag_should_propagate,
    r#"<cx ws>   <cx ws={false}>  <span>    </span>  <cx ws>    <div /> </cx></cx></cx>"#,
    r#"<cx ws>   <cx ws={false}><span></span><cx ws>    <div /> </cx></cx></cx>"#
);
