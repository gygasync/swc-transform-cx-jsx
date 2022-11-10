use std::borrow::BorrowMut;
use std::path::PathBuf;

use lazy_static::lazy_static;
use regex::Regex;
use swc_core::common::{Mark, DUMMY_SP};
use swc_core::ecma::ast::{
    ArrayLit, Expr, ExprOrSpread, Ident, JSXAttr, JSXAttrName, JSXAttrOrSpread, JSXAttrValue,
    JSXClosingElement, JSXElement, JSXElementChild, JSXElementName, JSXExpr, JSXExprContainer,
    JSXOpeningElement, JSXText, KeyValueProp, Lit, Null, ObjectLit, Prop, PropName, PropOrSpread,
    Str,
};
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

fn create_key_value_prop(key: String, value: Box<Expr>) -> PropOrSpread {
    PropOrSpread::Prop(Box::from(Prop::KeyValue(KeyValueProp {
        key: PropName::Str(Str::from(key)),
        value,
    })))
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

lazy_static! {
    static ref NULL_LIT_EXPR: Expr = Expr::Lit(Lit::Null(Null { span: DUMMY_SP }));
    static ref DASH_REGEX: Regex = Regex::new(r"(.*)-(bind|tpl|expr)").unwrap();
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"\s+").unwrap();
}

impl TransformVisitor {
    fn transform_cx_element(&mut self, expr: &mut Expr) -> Expr {
        match expr {
            Expr::JSXElement(jsx_el) => {
                let children: &mut Vec<JSXElementChild> = &mut jsx_el.children;
                let opening: JSXOpeningElement = jsx_el.opening.clone();
                let closing: &mut Option<JSXClosingElement> = &mut jsx_el.closing;

                if let JSXElementName::Ident(ident) = &opening.name {
                    let tag_name: String = ident.sym.to_string();
                    println!("TAG {}", tag_name);
                    if tag_name == "cx" || tag_name == "Cx" {
                        let mut transformed_children = children
                            .iter_mut()
                            .filter(|child| match *child {
                                JSXElementChild::JSXElement(..) => true,
                                _ => false,
                            })
                            .map(|child| self.transform_cx_child(child, false))
                            .filter(|child| child.is_some())
                            .map(|child| child.unwrap())
                            .collect::<Vec<_>>();

                        let transformed_children_expr = transformed_children
                            .iter_mut()
                            .map(|child| match child {
                                JSXElementChild::JSXElement(el) => {
                                    Some(ExprOrSpread::from(Expr::JSXElement(el.to_owned())))
                                }
                                JSXElementChild::JSXText(jsx_text) => Some(ExprOrSpread::from(
                                    Expr::Lit(Lit::JSXText(jsx_text.to_owned())),
                                )),
                                JSXElementChild::JSXExprContainer(jsx_expr_cont) => {
                                    Some(ExprOrSpread::from(match &jsx_expr_cont.expr {
                                        JSXExpr::Expr(expr) => self.transform_cx_element(
                                            expr.to_owned().unwrap_parens_mut(),
                                        ),
                                        JSXExpr::JSXEmptyExpr(_) => NULL_LIT_EXPR.to_owned(),
                                    }))
                                }
                                _ => panic!("Error creating Cx react element"),
                            })
                            .collect::<Vec<_>>();

                        let transformed_children_array_expr = Expr::Array(ArrayLit {
                            span: DUMMY_SP,
                            elems: transformed_children_expr,
                        });

                        return match (transformed_children.len()) {
                            0 => NULL_LIT_EXPR.to_owned(),
                            1 => {
                                let only_child = transformed_children[0].borrow_mut();

                                return match only_child {
                                    JSXElementChild::JSXElement(jsx_el) => {
                                        Expr::JSXElement(jsx_el.to_owned())
                                    }
                                    JSXElementChild::JSXFragment(jsx_fragment) => {
                                        Expr::JSXFragment((jsx_fragment.to_owned()))
                                    }
                                    JSXElementChild::JSXText(jsx_text) => {
                                        Expr::Lit(Lit::from(jsx_text.clone()))
                                    }
                                    JSXElementChild::JSXExprContainer(jsx_expr_cont) => {
                                        match &jsx_expr_cont.expr {
                                            JSXExpr::Expr(expr) => *expr.to_owned(),
                                            JSXExpr::JSXEmptyExpr(_) => NULL_LIT_EXPR.to_owned(),
                                        }
                                    }
                                    _ => panic!("Failed transforming only child."),
                                };
                            }
                            _ => transformed_children_array_expr,
                        };
                    }

                    let dot_index = tag_name.find('.');
                    let mut attrs: Vec<PropOrSpread> = vec![];
                    let tag_first_char = tag_name.get(0..1).unwrap();

                    if dot_index.is_some() {
                    } else if tag_first_char.to_lowercase() == tag_first_char {
                        // HtmlElement
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
                            JSXAttrOrSpread::SpreadElement(spread_el) => {
                                spread.push(Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(
                                        self.transform_cx_element(&mut spread_el.expr.clone()),
                                    ),
                                }));
                            }
                            JSXAttrOrSpread::JSXAttr(jsx_attr) => {
                                let processed = self.transform_cx_attribute(jsx_attr.clone());
                                attrs.push(PropOrSpread::Prop(Box::new(processed.to_owned())));
                                let attr_name = match processed {
                                    Prop::KeyValue(kv) => self.get_prop_name(&kv),
                                    _ => panic!("cannot parse attr_names Prop"),
                                };

                                attr_names.push(attr_name);
                            }
                        });
                    }

                    if !attr_names.is_empty() {
                        attrs.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: PropName::Str("jsxAttributes".into()),
                            value: Box::new(Expr::Array(ArrayLit {
                                span: DUMMY_SP,
                                elems: attr_names
                                    .into_iter()
                                    .map(|a| {
                                        Some(ExprOrSpread {
                                            spread: None,
                                            expr: Box::new(Expr::Lit(Lit::Str(
                                                a.to_owned().into(),
                                            ))),
                                        })
                                    })
                                    .collect::<Vec<_>>(),
                            })),
                        }))))
                    }

                    if !children.is_empty() {
                        let mut new_children: Vec<JSXElementChild> = vec![];
                    }

                    return Expr::Object(ObjectLit {
                        span: DUMMY_SP,
                        props: attrs,
                    });
                }

                expr.to_owned()
            }
            _ => expr.to_owned(),
        }
    }

    fn get_prop_name(&mut self, kv_prop: &KeyValueProp) -> String {
        match &kv_prop.key {
            PropName::Ident(ident) => ident.sym.to_string(),
            PropName::Str(str) => str.value.to_string(),
            PropName::Num(num) => num.value.to_string(),
            PropName::BigInt(big_int) => big_int.value.to_string(),
            _ => panic!("cannot parse attr_names prop keyValue"),
        }
    }

    fn transform_cx_attribute(&mut self, attr: JSXAttr) -> Prop {
        match &attr.value {
            Some(value) => match value {
                JSXAttrValue::Lit(lit) => {
                    self.generate_cx_property(attr.name, Box::new(Expr::Lit(lit.to_owned())))
                }
                JSXAttrValue::JSXElement(jsx_element) => {
                    let processed =
                        self.transform_cx_element(&mut Expr::JSXElement(jsx_element.to_owned()));

                    self.generate_cx_property(attr.name, Box::new(processed))
                }
                _ => self.generate_cx_property(attr.name, Box::new(NULL_LIT_EXPR.to_owned())),
            },
            None => {
                self.generate_cx_property(attr.name, Box::new(Expr::Lit(Lit::Bool(true.into()))))
            }
        }
    }

    fn generate_cx_property(&mut self, name: JSXAttrName, value: Box<Expr>) -> Prop {
        match name {
            JSXAttrName::JSXNamespacedName(ns_name) => Prop::KeyValue(KeyValueProp {
                key: PropName::Ident(ns_name.ns),
                value: Box::from(
                    self.bind_cx_expr_tpl_object(&ns_name.name.sym.to_string(), value),
                ),
            }),
            JSXAttrName::Ident(ident) => {
                let symbol = ident.sym.to_string();

                let regex_matches = DASH_REGEX.captures(&symbol);
                match regex_matches {
                    Some(matches) => {
                        let (_, ident) = obj_key_identifier(&matches[1]);

                        if ident.is_some() {
                            return Prop::KeyValue(KeyValueProp {
                                key: PropName::Ident(ident.unwrap()),
                                value: Box::new(self.bind_cx_expr_tpl_object(&matches[2], value)),
                            });
                        } else {
                            return Prop::KeyValue(KeyValueProp {
                                key: PropName::Str(symbol.into()),
                                value,
                            });
                        }
                    }
                    None => Prop::KeyValue(KeyValueProp {
                        key: PropName::Str(symbol.into()),
                        value,
                    }),
                }
            }
        }
    }

    fn bind_cx_expr_tpl_object(&mut self, instr: &str, value: Box<Expr>) -> Expr {
        let mut local_value: Box<Expr> = value.clone();
        if instr == "expr" && false {
            todo!("fat arrows")
        } // TODO: Fat arrows

        let identifiers = obj_key_identifier(instr);
        let mut key: PropName;

        // TODO: Make this a lot cleaner
        if identifiers.0.is_some() {
            key = PropName::Str(identifiers.0.unwrap());
        } else {
            key = PropName::Ident(identifiers.1.unwrap());
        }

        let prop = Prop::KeyValue(KeyValueProp {
            key,
            value: local_value,
        });
        let props: Vec<PropOrSpread> = vec![PropOrSpread::Prop(Box::from(prop))];

        return Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props,
        });
    }

    fn transform_cx_child(
        &mut self,
        child: &mut JSXElementChild,
        preserve_whitespace: bool,
    ) -> Option<JSXElementChild> {
        match child {
            JSXElementChild::JSXText(jsx_text) => {
                if preserve_whitespace {
                    return Some(child.to_owned());
                } else {
                    let inner_text = jsx_text.value.to_string();
                    let result = WHITESPACE_REGEX
                        .replace_all(inner_text.as_str(), "")
                        .into_owned();

                    if result.is_empty() {
                        return None;
                    }

                    return Some(JSXElementChild::JSXText(JSXText {
                        span: DUMMY_SP,
                        value: result.clone().into(),
                        raw: result.escape_default().to_string().into(),
                    }));
                }
            }
            JSXElementChild::JSXElement(jsx_element) => {
                Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(self.transform_cx_element(
                        &mut Expr::JSXElement(Box::new(*jsx_element.to_owned())),
                    ))),
                }))
            }
            _ => Some(child.to_owned()),
        }
    }
}

impl VisitMut for TransformVisitor {
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        // match expr.borrow() {
        //     Expr::JSXElement(jsx_el) => {

        //     }
        //     _ => {}
        // }

        if let Expr::JSXElement(jsx_el) = expr {
            if let JSXElementName::Ident(ident) = &mut jsx_el.opening.name {
                let tag_name = ident.sym.to_string();
                if tag_name == "cx" || tag_name == "Cx" {
                    *expr = self.transform_cx_element(expr)
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
