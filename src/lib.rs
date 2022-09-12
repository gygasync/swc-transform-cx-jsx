use std::borrow::Borrow;
use std::path::PathBuf;
use std::ptr::null;
use std::vec;

use regex::Regex;
use serde::Serialize;
use swc_common::chain;
use swc_common::{util::take::Take, DUMMY_SP};
use swc_core::testing_transform::test_fixture;
use swc_core::visit::Fold;
use swc_core::{
    ast::Ident,
    ast::Program,
    plugin::{plugin_transform, proxies::TransformPluginProgramMetadata},
    testing_transform::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
};
use swc_core::{ast::*, common::EqIgnoreSpan};
use swc_ecma_parser::{EsConfig, Syntax};

fn transform_cx_element(el: Box<JSXElement>) -> Expr {
    cx_process_element(Expr::JSXElement(el))
}

fn get_prop_name(kv_prop: &KeyValueProp) -> String {
    match &kv_prop.key {
        PropName::Ident(ident) => ident.sym.to_string(),
        PropName::Str(str) => str.value.to_string(),
        PropName::Num(num) => num.value.to_string(),
        PropName::BigInt(big_int) => big_int.value.to_string(),
        _ => panic!("cannot parse attr_names prop keyValue"),
    }
}

fn cx_process_element(expr: Expr) -> Expr {
    return match expr {
        Expr::JSXElement(el) => {
            let mut children: Vec<JSXElementChild> = el.children.clone();
            let opening: JSXOpeningElement = el.opening.clone();
            let closing: Option<JSXClosingElement> = el.closing.clone();

            let tag_name = element_name(*el);

            if tag_name == "cx" || tag_name == "Cx" {
                let transformed_children = children
                    .iter()
                    .filter(|&c| match *c {
                        JSXElementChild::JSXElement(..) => true,
                        _ => false,
                    })
                    .map(|c| cx_process_child(c.clone(), false)) // Revisit all clone invokations, memory and performance improvmenets are hiding here
                    .filter(|c| c.is_some())
                    .map(|c| c.unwrap())
                    .collect::<Vec<_>>();

                let transformed_children_expr = transformed_children
                    .iter()
                    .map(|c| match c {
                        JSXElementChild::JSXElement(el) => {
                            Some(ExprOrSpread::from(Expr::JSXElement(el.clone())))
                        }
                        JSXElementChild::JSXText(jsx_text) => Some(ExprOrSpread::from(Expr::Lit(
                            Lit::Str(jsx_text.value.to_string().into()),
                        ))),
                        JSXElementChild::JSXExprContainer(expr) => {
                            Some(ExprOrSpread::from(match expr.expr.clone() {
                                JSXExpr::Expr(expr) => cx_process_element(*expr),
                                JSXExpr::JSXEmptyExpr(_) => {
                                    Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))
                                }
                            }))
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

                if transformed_children.is_empty() {
                    return Expr::Lit(Lit::Null(Null { span: DUMMY_SP }));
                }

                if transformed_children.len() == 1 {
                    let only_child = transformed_children[0].clone();
                    return match only_child {
                        JSXElementChild::JSXElement(el) => Expr::JSXElement(el),
                        JSXElementChild::JSXFragment(frag) => Expr::JSXFragment(frag),
                        JSXElementChild::JSXText(text) => Expr::Lit(Lit::from(text)),
                        JSXElementChild::JSXExprContainer(expr_container) => {
                            match expr_container.expr {
                                JSXExpr::JSXEmptyExpr(_) => {
                                    Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))
                                }
                                JSXExpr::Expr(expr) => *expr,
                            }
                        }
                        _ => panic!("Failed transforming only child"),
                    };
                }

                return transformed_children_array_expr;
            }

            let dot_index = tag_name.find('.');
            let mut attrs: Vec<PropOrSpread> = vec![];
            let tag_first_char = tag_name.get(0..1).unwrap();

            if dot_index.is_some() {
                let index = dot_index.unwrap();
                let before_dot: String = tag_name.chars().take(index).collect();
                let after_dot: String = tag_name
                    .chars()
                    .skip(index + 1)
                    .take(tag_name.len())
                    .collect();

                let left_ident = Ident {
                    span: DUMMY_SP,
                    sym: before_dot.into(),
                    optional: false,
                };
                let right_ident = Ident {
                    span: DUMMY_SP,
                    sym: after_dot.into(),
                    optional: false,
                };

                let dot_prop = create_key_value_prop(
                    String::from("$type"),
                    Box::from(Expr::Member(MemberExpr {
                        span: DUMMY_SP,
                        obj: Box::from(Expr::Ident(left_ident)),
                        prop: MemberProp::Ident(right_ident),
                    })),
                );
                attrs.push(dot_prop);
            } else if tag_first_char.to_lowercase() == tag_first_char {
                let html_element = create_key_value_prop(
                    String::from("$type"),
                    Box::from(Expr::Ident(Ident {
                        span: DUMMY_SP,
                        sym: "HtmlElement".into(),
                        optional: false,
                    })),
                );
                let tag = create_key_value_prop(
                    String::from("tag"),
                    Box::from(Expr::Lit(Lit::Str(tag_name.into()))),
                );
                attrs.push(html_element);
                attrs.push(tag);
            } else {
                let prop = create_key_value_prop(
                    String::from("$type"),
                    Box::from(Expr::Ident(Ident {
                        span: DUMMY_SP,
                        sym: tag_name.into(),
                        optional: false,
                    })),
                );
                attrs.push(prop);
            }

            let mut attr_names: Vec<String> = vec![];
            let mut spread: Vec<Option<ExprOrSpread>> = vec![];
            let attributes: Vec<JSXAttrOrSpread> = opening.attrs.clone();

            if !opening.attrs.is_empty() {
                attributes.iter().for_each(|a| match a {
                    JSXAttrOrSpread::SpreadElement(spread_el) => spread.push(Some(ExprOrSpread {
                        spread: Some(spread_el.dot3_token),
                        expr: Box::new(cx_process_element(*spread_el.expr.clone())),
                    })),
                    JSXAttrOrSpread::JSXAttr(jsx_attr) => {
                        let processed = cx_process_attribute(jsx_attr.clone());
                        attrs.push(PropOrSpread::Prop(Box::new(processed.clone())));
                        let attr_name = match processed.borrow() {
                            Prop::KeyValue(kv) => get_prop_name(kv),
                            _ => panic!("cannot parse attr_names Prop"),
                        };

                        attr_names.push(attr_name);
                    }
                });
            }

            if !spread.is_empty() {
                attrs.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: PropName::Str("jsxSpread".into()),
                    value: Box::new(Expr::Array(ArrayLit {
                        span: DUMMY_SP,
                        elems: spread,
                    })),
                }))))
            }

            if !attr_names.is_empty() {
                attrs.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: PropName::Str("jsxAttributes".into()),
                    value: Box::new(Expr::Array(ArrayLit {
                        span: DUMMY_SP,
                        elems: attr_names
                            .iter()
                            .map(|a| {
                                return Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(Expr::Lit(Lit::Str((*a.clone()).into()))),
                                });
                            })
                            .collect::<Vec<_>>(),
                    })),
                }))))
            }

            if !children.is_empty() {
                let mut new_children: Vec<JSXElementChild> = vec![];
                children.iter().for_each(|c| {
                    let child = cx_process_child(c.clone(), false);
                    if child.is_some() {
                        new_children.push(child.unwrap());
                    }
                });

                attrs.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: PropName::Str("children".into()),
                    value: Box::new(Expr::Array(ArrayLit {
                        span: DUMMY_SP,
                        elems: new_children
                            .iter()
                            .map(|nc| {
                                Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(match nc {
                                        JSXElementChild::JSXText(text) => {
                                            let str_lit =
                                                Lit::Str(text.value.clone().to_string().into());

                                            Expr::Lit(str_lit)
                                        }
                                        JSXElementChild::JSXElement(el) => {
                                            Expr::JSXElement(el.clone())
                                        }
                                        JSXElementChild::JSXSpreadChild(spread) => {
                                            *spread.expr.clone()
                                        }
                                        JSXElementChild::JSXExprContainer(expr) => {
                                            match expr.expr.borrow() {
                                                JSXExpr::JSXEmptyExpr(_) => {
                                                    Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))
                                                }
                                                JSXExpr::Expr(ex) => *ex.clone(),
                                            }
                                        }
                                        JSXElementChild::JSXFragment(frag) => {
                                            Expr::JSXFragment(frag.clone())
                                        }
                                    }),
                                })
                            })
                            .collect::<Vec<_>>(),
                    })),
                }))))
            }

            return Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props: attrs,
            });
        }
        Expr::Object(obj) => {
            let mut attrs: Vec<PropOrSpread> = vec![];

            obj.props.iter().for_each(|o| match o.borrow() {
                PropOrSpread::Prop(prop) => match prop.borrow() {
                    Prop::KeyValue(key_value) => {
                        let prop_name = get_prop_name(key_value);
                        let value = cx_process_element(*key_value.value.clone());
                        attrs.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: PropName::Str(prop_name.into()),
                            value: Box::new(value),
                        }))))
                    }
                    _ => todo!("Only keyvalue is implemented for obj props"),
                },
                PropOrSpread::Spread(spread) => {}
            });
            return Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props: attrs,
            });
        }
        _ => expr,
    };
}

fn process_expr_obj_props(props: Vec<PropOrSpread>) -> Vec<PropOrSpread> {
    props
        .iter()
        .map(|p| {
            let tx = match p.borrow() {
                PropOrSpread::Spread(spread) => PropOrSpread::Spread(SpreadElement {
                    dot3_token: (*spread).dot3_token,
                    expr: Box::new(cx_process_element(*spread.expr.clone())),
                }),
                PropOrSpread::Prop(prop) => match prop.borrow() {
                    Prop::KeyValue(kv) => {
                        PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: kv.key.clone(),
                            value: Box::new(cx_process_element(*kv.value.clone())),
                        })))
                    }
                    _ => PropOrSpread::Prop(prop.clone()),
                },
            };

            return tx;
        })
        .collect::<Vec<_>>()
}

fn cx_process_child(child: JSXElementChild, preserve_whitespace: bool) -> Option<JSXElementChild> {
    return match child.borrow() {
        JSXElementChild::JSXText(jsx_text) => {
            return if preserve_whitespace {
                Some(child)
            } else {
                let inner_text = jsx_text.value.to_string();
                let regex = regex::Regex::new(r"\s+").unwrap();
                let result = regex.replace_all(inner_text.as_str(), "").into_owned();

                if result.is_empty() {
                    return None;
                }

                Some(JSXElementChild::JSXText(JSXText {
                    span: DUMMY_SP,
                    value: result.clone().into(),
                    raw: result.escape_default().to_string().into(),
                }))
            }
        }
        JSXElementChild::JSXElement(el) => {
            Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                span: DUMMY_SP,
                expr: JSXExpr::Expr(Box::from(cx_process_element(Expr::JSXElement(el.clone())))),
            }))
        }
        JSXElementChild::JSXExprContainer(expr) => match expr.expr.borrow() {
            JSXExpr::JSXEmptyExpr(_) => Some(child),
            JSXExpr::Expr(e) => match *e.clone() {
                Expr::Object(obj) => {
                    let transformed_values = process_expr_obj_props(obj.props);

                    return Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::new(Expr::Object(ObjectLit {
                            span: DUMMY_SP,
                            props: transformed_values,
                        }))),
                    }));
                }
                Expr::Array(array) => Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(Expr::Array(ArrayLit {
                        span: DUMMY_SP,
                        elems: cx_process_expr_array_elems(array.elems),
                    }))),
                })),
                _ => Some(child),
            },
        },
        _ => Some(child),
    };
}

fn cx_process_expr_array_elems(props: Vec<Option<ExprOrSpread>>) -> Vec<Option<ExprOrSpread>> {
    props
        .iter()
        .map(|p| match p {
            None => p.clone(),
            Some(eos) => Some(ExprOrSpread {
                spread: eos.spread,
                expr: Box::new(cx_process_element(*eos.expr.clone())),
            }),
        })
        .collect::<Vec<_>>()
}

fn cx_process_attribute(attr: JSXAttr) -> Prop {
    match attr.value {
        Some(value) => match value {
            JSXAttrValue::Lit(lit) => cx_property(attr.name, Box::new(Expr::Lit(lit))),
            JSXAttrValue::JSXElement(jsx_el) => {
                let processed = cx_process_element(Expr::JSXElement(jsx_el));

                cx_property(attr.name, Box::from(processed))
            }
            JSXAttrValue::JSXExprContainer(expr) => match expr.expr {
                JSXExpr::JSXEmptyExpr(_) => todo!("Exmpty prop expression"),
                JSXExpr::Expr(e) => {
                    let processed = cx_process_element(*e);
                    return cx_property(attr.name, Box::from(processed));
                }
            },
            _ => todo!("asd"),
        },
        None => cx_property(attr.name, Box::from(Expr::Lit(Lit::Bool(true.into())))),
    }
}

fn cx_property(name: JSXAttrName, value: Box<Expr>) -> Prop {
    lazy_static::lazy_static! {
        static ref dash_regex: Regex = Regex::new(r"(.*)-(bind|tpl|expr)").unwrap();
    }

    match name {
        JSXAttrName::JSXNamespacedName(nm_name) => Prop::KeyValue(KeyValueProp {
            key: PropName::Ident(nm_name.ns),
            value: Box::from(cx_bind_expr_tpl_object(
                &nm_name.name.sym.to_string(),
                value,
            )),
        }),
        JSXAttrName::Ident(ident) => {
            let symbol = ident.sym.to_string();

            if dash_regex.is_match(symbol.as_str()) {
                let regex_matches = &dash_regex.captures(symbol.as_str()).unwrap();

                let (_, ident) = obj_key_identifier(&regex_matches[1]);

                if ident.is_some() {
                    return Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(ident.unwrap()),
                        value: Box::new(cx_bind_expr_tpl_object(&regex_matches[2], value)),
                    });
                }
            }

            return Prop::KeyValue(KeyValueProp {
                key: PropName::Ident(ident),
                value: value,
            });
        }
    }
}

fn cx_bind_expr_tpl_object(instr: &str, value: Box<Expr>) -> Expr {
    let mut local_value: Box<Expr> = value.clone();
    if instr == "expr" && false {
        // TODO: implement fat arrows
        match *value {
            Expr::Lit(lit) => match lit {
                Lit::Str(str_lit) => {}
                _ => {}
            },
            _ => {}
        }
    }
    let identifiers = obj_key_identifier(instr);
    let mut key: PropName;
    if identifiers.0.is_some() {
        key = PropName::Str(identifiers.0.unwrap());
    } else {
        key = PropName::Ident(identifiers.1.unwrap());
    }

    let prop = Prop::KeyValue(KeyValueProp {
        key,
        value: local_value,
    });

    let result: Vec<PropOrSpread> = vec![PropOrSpread::Prop(Box::from(prop))];
    return create_object_expr(result);
}

impl VisitMut for TransformVisitor {
    // fn visit_mut_jsx_element_children(&mut self, children: &mut Vec<JSXElementChild>) {

    // }

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        match expr.borrow() {
            Expr::JSXElement(jsx_el) => {
                let tag_name = element_name(*jsx_el.clone());
                if tag_name == "cx" || tag_name == "Cx" {
                    *expr = transform_cx_element(jsx_el.clone());
                }
            }
            _ => {}
        }
    }
}

pub struct TransformVisitor;

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    let program = program.fold_with(&mut as_folder(TransformVisitor));
    program
}

// pub fn transform_cx(options: CxOptions) {
//     let mut folder = CxImports { options };

//     folder
// }

#[derive(Clone, Copy)]
pub struct CxOptions {
    trimWhitespace: bool,
}

struct CxImports {
    options: CxOptions,
}

fn obj_key_identifier(name: &str) -> (Option<Str>, Option<Ident>) {
    let mut str: Option<Str> = None;
    let mut ident: Option<Ident> = None;

    if name.find('-').is_some() {
        str = Some(Str::from(name));
    } else {
        ident = Some(Ident {
            span: DUMMY_SP,
            sym: name.into(),
            optional: false,
        });
    }

    return (str, ident);
}

fn get_name_from_jsx_obj(object: JSXObject) -> String {
    match object {
        JSXObject::JSXMemberExpr(expr) => get_name_from_jsx_obj(expr.obj),
        JSXObject::Ident(ident) => ident.sym.to_string(),
    }
}

fn el_name_from_jsx_name(name: JSXElementName) -> String {
    match name {
        JSXElementName::Ident(ident) => return ident.sym.to_string(),
        JSXElementName::JSXMemberExpr(expr) => {
            return String::from(format!(
                "{}.{}",
                get_name_from_jsx_obj(expr.obj),
                expr.prop.sym.to_string()
            ))
        }
        _ => panic!("Could not calculate name."),
    }
}

fn element_name(element: JSXElement) -> String {
    el_name_from_jsx_name(element.opening.name)
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

// test!(
//     swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsConfig {
//         jsx: true,
//         ..Default::default()
//     }),
//     |_| as_folder(TransformVisitor),
//     ws_flag_preserves_whitespace_for_children,
//     r#"<cx></cx>"#,
//     r#"null;"#
// );

#[testing::fixture("tests/**/input.js")]
fn exec(input: PathBuf) {
    let output = input.with_file_name("output.js");
    test_fixture(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        &|_| chain!(as_folder(TransformVisitor), as_folder(TransformVisitor)), // This works but i do not know how and why
        &input,
        &output,
    )
}
