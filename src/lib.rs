use std::borrow::Borrow;
use std::ptr::null;
use std::vec;

use regex::Regex;
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

fn element_name(element: JSXElement) -> String {
    match element.opening.name {
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

impl CxImports {
    fn transform_jsx_child_element(&mut self, element: JSXElementChild) -> JSXElementChild {
        match element {
            JSXElementChild::JSXElement(el) => {
                let transformed_el = self.process_element(Expr::JSXElement(el));
                match transformed_el {
                    Expr::JSXElement(jsx_el) => return JSXElementChild::JSXElement(jsx_el),
                    _ => panic!("Unable to process JSX Element Child"),
                }
            }
            _ => panic!("Unable to process JSX element"),
        }
    }

    fn process_attribute(&mut self, attr: JSXAttr) -> Prop {
        match attr.value {
            Some(value) => todo!("asd"),
            None => self.property(attr.name, Box::from(Expr::Lit(Lit::Bool(true.into())))),
        }
    }

    fn process_expr_obj_props(&mut self, props: Vec<PropOrSpread>) -> Vec<PropOrSpread> {
        props
            .iter()
            .map(|p| {
                let tx = match p.borrow() {
                    PropOrSpread::Spread(spread) => PropOrSpread::Spread(SpreadElement {
                        dot3_token: (*spread).dot3_token,
                        expr: Box::new(self.process_element(*spread.expr.clone())),
                    }),
                    PropOrSpread::Prop(prop) => match prop.borrow() {
                        Prop::KeyValue(kv) => {
                            PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                                key: kv.key.clone(),
                                value: Box::new(self.process_element(*kv.value.clone())),
                            })))
                        }
                        _ => PropOrSpread::Prop(prop.clone()),
                    },
                };

                return tx;
            })
            .collect::<Vec<_>>()
    }

    fn process_expr_array_elems(
        &mut self,
        props: Vec<Option<ExprOrSpread>>,
    ) -> Vec<Option<ExprOrSpread>> {
        props
            .iter()
            .map(|p| match p {
                None => p.clone(),
                Some(eos) => Some(ExprOrSpread {
                    spread: eos.spread,
                    expr: Box::new(self.process_element(*eos.expr.clone())),
                }),
            })
            .collect::<Vec<_>>()
    }

    fn process_child(
        &mut self,
        child: JSXElementChild,
        preserveWhitespace: bool,
    ) -> JSXElementChild {
        return match child.borrow() {
            JSXElementChild::JSXText(jsx_text) => {
                return if preserveWhitespace {
                    child
                } else {
                    let inner_text = jsx_text.value.to_string();
                    let regex = regex::Regex::new(r"\s+").unwrap();
                    let result = regex.replace_all(inner_text.as_str(), "g").into_owned();
                    JSXElementChild::JSXText(JSXText {
                        span: DUMMY_SP,
                        value: result.clone().into(),
                        raw: result.escape_default().to_string().into(),
                    })
                }
            }
            JSXElementChild::JSXElement(el) => {
                JSXElementChild::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::from(
                        self.process_element(Expr::JSXElement(el.clone())),
                    )),
                })
            }
            JSXElementChild::JSXExprContainer(expr) => match expr.expr.borrow() {
                JSXExpr::JSXEmptyExpr(_) => child,
                JSXExpr::Expr(e) => match *e.clone() {
                    Expr::Object(obj) => {
                        let transformed_values = self.process_expr_obj_props(obj.props);

                        return JSXElementChild::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::new(Expr::Object(ObjectLit {
                                span: DUMMY_SP,
                                props: transformed_values,
                            }))),
                        });
                    }
                    Expr::Array(array) => JSXElementChild::JSXExprContainer(JSXExprContainer {
                        span: DUMMY_SP,
                        expr: JSXExpr::Expr(Box::new(Expr::Array(ArrayLit {
                            span: DUMMY_SP,
                            elems: self.process_expr_array_elems(array.elems),
                        }))),
                    }),
                    _ => child,
                },
            },
            _ => child,
        };
    }

    fn bind_expr_tpl_object(&mut self, instr: &str, value: Box<Expr>) -> Expr {
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

    fn property(&mut self, name: JSXAttrName, value: Box<Expr>) -> Prop {
        match name {
            JSXAttrName::JSXNamespacedName(nm_name) => Prop::KeyValue(KeyValueProp {
                key: PropName::Ident(nm_name.ns),
                value: Box::from(self.bind_expr_tpl_object(&nm_name.name.sym.to_string(), value)),
            }),
            JSXAttrName::Ident(ident) => Prop::KeyValue(KeyValueProp {
                key: PropName::Ident(ident),
                value: value,
            }),
        }
    }

    fn process_element(&mut self, expr: Expr) -> Expr {
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

                let attr_names: Vec<String> = vec![];
                let mut spread: Vec<Option<ExprOrSpread>> = vec![];
                let attributes: Vec<JSXAttrOrSpread> = opening.attrs.clone();

                if !opening.attrs.is_empty() {
                    attributes.iter().for_each(|a| match a {
                        JSXAttrOrSpread::SpreadElement(spread_el) => {
                            spread.push(Some(ExprOrSpread {
                                spread: Some(spread_el.dot3_token),
                                expr: spread_el.expr.clone(),
                            }))
                        }
                        JSXAttrOrSpread::JSXAttr(jsx_attr) => {
                            let processed = self.process_attribute(jsx_attr.clone());
                            attrs.push(PropOrSpread::Prop(Box::new(processed.clone())));
                            let attr_name = match processed.borrow() {
                                Prop::KeyValue(kv) => match &kv.key {
                                    PropName::Ident(ident) => ident.sym.to_string(),
                                    PropName::Str(str) => str.value.to_string(),
                                    PropName::Num(num) => num.value.to_string(),
                                    PropName::BigInt(big_int) => big_int.value.to_string(),
                                    _ => panic!("cannot parse attr_names prop keyValue"),
                                },
                                _ => panic!("cannot parse attr_names Prop"),
                            };
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
                        let child = self.process_child(c.clone(), false);
                        new_children.push(child);
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
                                                        Expr::Lit(Lit::Null(Null {
                                                            span: DUMMY_SP,
                                                        }))
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
            _ => expr,
        };
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
