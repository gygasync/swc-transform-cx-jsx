use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::path::PathBuf;
use std::vec;

use regex::Regex;
use swc_common::{chain, hygiene::Mark, util::take::Take, DUMMY_SP};
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
use swc_ecma_transforms_base::resolver;

impl TransformVisitor {
    fn transform_cx_element(&mut self, el: Box<JSXElement>) -> Expr {
        self.cx_process_element(Expr::JSXElement(el))
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

    fn cx_process_element(&mut self, expr: Expr) -> Expr {
        return match expr {
            Expr::JSXElement(el) => {
                let children: Vec<JSXElementChild> = el.children.clone();
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
                        .map(|c| self.cx_process_child(c.clone(), false)) // Revisit all clone invokations, memory and performance improvmenets are hiding here
                        .filter(|c| c.is_some())
                        .map(|c| c.unwrap())
                        .collect::<Vec<_>>();

                    let transformed_children_expr = transformed_children
                        .iter()
                        .map(|c| match c {
                            JSXElementChild::JSXElement(el) => {
                                Some(ExprOrSpread::from(Expr::JSXElement(el.clone())))
                            }
                            JSXElementChild::JSXText(jsx_text) => Some(ExprOrSpread::from(
                                Expr::Lit(Lit::Str(jsx_text.value.to_string().into())),
                            )),
                            JSXElementChild::JSXExprContainer(expr) => {
                                Some(ExprOrSpread::from(match expr.expr.clone() {
                                    JSXExpr::Expr(expr) => self.cx_process_element(*expr),
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
                        if !children.is_empty() {
                            let items_container = JSXExprContainer {
                                span: DUMMY_SP,
                                expr: JSXExpr::Expr(Box::from(transformed_children_array_expr)),
                            };

                            let items_attr = create_jsx_attribute(
                                "items",
                                Option::from(JSXAttrValue::JSXExprContainer(items_container)),
                            );
                            attrs.push(JSXAttrOrSpread::JSXAttr(items_attr));
                        }

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
                    if self.imports.contains_key("cx/widgets") {
                        self.imports
                            .get_mut("cx/widgets")
                            .unwrap()
                            .insert("HtmlElement".into());
                    } else {
                        let mut import_set: HashSet<String> = HashSet::new();
                        import_set.insert("HtmlElement".into());
                        self.imports.insert("cx/widgets".into(), import_set);
                    }
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
                        JSXAttrOrSpread::SpreadElement(spread_el) => {
                            spread.push(Some(ExprOrSpread {
                                // spread: Some(spread_el.dot3_token),
                                spread: None,
                                expr: Box::new(self.cx_process_element(*spread_el.expr.clone())),
                            }));
                        }
                        JSXAttrOrSpread::JSXAttr(jsx_attr) => {
                            let processed = self.cx_process_attribute(jsx_attr.clone());
                            attrs.push(PropOrSpread::Prop(Box::new(processed.clone())));
                            let attr_name = match processed.borrow() {
                                Prop::KeyValue(kv) => self.get_prop_name(kv),
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
                        let child = self.cx_process_child(c.clone(), false);
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
            Expr::Object(obj) => {
                let mut attrs: Vec<PropOrSpread> = vec![];

                obj.props.iter().for_each(|o| match o.borrow() {
                    PropOrSpread::Prop(prop) => match prop.borrow() {
                        Prop::KeyValue(key_value) => {
                            let prop_name = self.get_prop_name(key_value);
                            let value = self.cx_process_element(*key_value.value.clone());
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
            Expr::Array(array) => {
                let mut elems: Vec<Option<ExprOrSpread>> = vec![];

                array.elems.iter().for_each(|e| match e.borrow() {
                    Some(expr_or_spread) => elems.push(Some(ExprOrSpread {
                        spread: None,
                        expr: Box::new(self.cx_process_element(*expr_or_spread.expr.clone())),
                    })),
                    None => {}
                });

                return Expr::Array(ArrayLit {
                    span: DUMMY_SP,
                    elems,
                });
            }
            _ => expr,
        };
    }

    fn process_expr_obj_props(&mut self, props: Vec<PropOrSpread>) -> Vec<PropOrSpread> {
        props
            .iter()
            .map(|p| {
                let tx = match p.borrow() {
                    PropOrSpread::Spread(spread) => PropOrSpread::Spread(SpreadElement {
                        dot3_token: (*spread).dot3_token,
                        expr: Box::new(self.cx_process_element(*spread.expr.clone())),
                    }),
                    PropOrSpread::Prop(prop) => match prop.borrow() {
                        Prop::KeyValue(kv) => {
                            PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                                key: kv.key.clone(),
                                value: Box::new(self.cx_process_element(*kv.value.clone())),
                            })))
                        }
                        _ => PropOrSpread::Prop(prop.clone()),
                    },
                };

                return tx;
            })
            .collect::<Vec<_>>()
    }

    fn cx_process_child(
        &mut self,
        child: JSXElementChild,
        preserve_whitespace: bool,
    ) -> Option<JSXElementChild> {
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
                    expr: JSXExpr::Expr(Box::from(
                        self.cx_process_element(Expr::JSXElement(el.clone())),
                    )),
                }))
            }
            JSXElementChild::JSXExprContainer(expr) => match expr.expr.borrow() {
                JSXExpr::JSXEmptyExpr(_) => Some(child),
                JSXExpr::Expr(e) => match *e.clone() {
                    Expr::Object(obj) => {
                        let transformed_values = self.process_expr_obj_props(obj.props);

                        return Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::new(Expr::Object(ObjectLit {
                                span: DUMMY_SP,
                                props: transformed_values,
                            }))),
                        }));
                    }
                    Expr::Array(array) => {
                        Some(JSXElementChild::JSXExprContainer(JSXExprContainer {
                            span: DUMMY_SP,
                            expr: JSXExpr::Expr(Box::new(Expr::Array(ArrayLit {
                                span: DUMMY_SP,
                                elems: self.cx_process_expr_array_elems(array.elems),
                            }))),
                        }))
                    }
                    _ => Some(child),
                },
            },
            _ => Some(child),
        };
    }

    fn cx_process_expr_array_elems(
        &mut self,
        props: Vec<Option<ExprOrSpread>>,
    ) -> Vec<Option<ExprOrSpread>> {
        props
            .iter()
            .map(|p| match p {
                None => p.clone(),
                Some(eos) => Some(ExprOrSpread {
                    spread: eos.spread,
                    expr: Box::new(self.cx_process_element(*eos.expr.clone())),
                }),
            })
            .collect::<Vec<_>>()
    }

    fn cx_process_attribute(&mut self, attr: JSXAttr) -> Prop {
        match attr.value {
            Some(value) => match value {
                JSXAttrValue::Lit(lit) => self.cx_property(attr.name, Box::new(Expr::Lit(lit))),
                JSXAttrValue::JSXElement(jsx_el) => {
                    let processed = self.cx_process_element(Expr::JSXElement(jsx_el));

                    self.cx_property(attr.name, Box::from(processed))
                }
                JSXAttrValue::JSXExprContainer(expr) => match expr.expr {
                    JSXExpr::JSXEmptyExpr(_) => todo!("Exmpty prop expression"),
                    JSXExpr::Expr(e) => {
                        let processed = match e.borrow() {
                            // Arrow fn has to be handled here instead of cx_process_element, the ArrowFn does not contain an identifier
                            // But the FuncExpr requires an identifier which will be generated from the attribute name
                            Expr::Arrow(arrow_fn) => {
                                let fn_ident = match attr.name.borrow() {
                                    JSXAttrName::Ident(ident) => ident,
                                    JSXAttrName::JSXNamespacedName(ns_name) => {
                                        todo!("JSX NAMESPACED NAME AS ARROWFN IDENTIFIER")
                                    }
                                };

                                let transformed_params = arrow_fn
                                    .params
                                    .clone()
                                    .iter()
                                    .map(|p| Param {
                                        span: DUMMY_SP,
                                        pat: p.clone(),
                                        decorators: vec![],
                                    })
                                    .collect::<Vec<_>>();

                                let transformed_body = match arrow_fn.clone().body {
                                    BlockStmtOrExpr::BlockStmt(block_stmt) => block_stmt,
                                    BlockStmtOrExpr::Expr(expr) => BlockStmt {
                                        span: DUMMY_SP,
                                        stmts: vec![Stmt::Expr(ExprStmt {
                                            span: DUMMY_SP,
                                            expr: expr,
                                        })],
                                    },
                                };

                                Expr::Fn(FnExpr {
                                    ident: Some(fn_ident.clone()),
                                    function: Function {
                                        params: transformed_params,
                                        decorators: vec![],
                                        span: DUMMY_SP,
                                        body: Some(transformed_body),
                                        is_generator: arrow_fn.is_generator,
                                        is_async: arrow_fn.is_async,
                                        type_params: arrow_fn.type_params.clone(),
                                        return_type: arrow_fn.return_type.clone(),
                                    },
                                })
                            }
                            _ => self.cx_process_element(*e),
                        };

                        return self.cx_property(attr.name, Box::from(processed));
                    }
                },
                _ => todo!("asd"),
            },
            None => self.cx_property(attr.name, Box::from(Expr::Lit(Lit::Bool(true.into())))),
        }
    }

    fn cx_property(&mut self, name: JSXAttrName, value: Box<Expr>) -> Prop {
        lazy_static::lazy_static! {
            static ref dash_regex: Regex = Regex::new(r"(.*)-(bind|tpl|expr)").unwrap();
        }

        match name {
            JSXAttrName::JSXNamespacedName(nm_name) => Prop::KeyValue(KeyValueProp {
                key: PropName::Ident(nm_name.ns),
                value: Box::from(
                    self.cx_bind_expr_tpl_object(&nm_name.name.sym.to_string(), value),
                ),
            }),
            JSXAttrName::Ident(ident) => {
                let symbol = ident.sym.to_string();

                if dash_regex.is_match(symbol.as_str()) {
                    let regex_matches = &dash_regex.captures(symbol.as_str()).unwrap();

                    let (_, ident) = obj_key_identifier(&regex_matches[1]);

                    if ident.is_some() {
                        return Prop::KeyValue(KeyValueProp {
                            key: PropName::Ident(ident.unwrap()),
                            value: Box::new(self.cx_bind_expr_tpl_object(&regex_matches[2], value)),
                        });
                    }
                }

                return Prop::KeyValue(KeyValueProp {
                    key: PropName::Str(symbol.into()),
                    value: value,
                });
            }
        }
    }

    fn cx_bind_expr_tpl_object(&mut self, instr: &str, value: Box<Expr>) -> Expr {
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
}

impl VisitMut for TransformVisitor {
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        match expr.borrow() {
            Expr::Arrow(arrow_fn_expr) => match arrow_fn_expr.body.borrow() {
                BlockStmtOrExpr::Expr(body_expr) => match body_expr.borrow() {
                    Expr::Paren(paren_expr) => match *paren_expr.expr.clone() {
                        Expr::JSXElement(jsx_el) => {
                            println!("Arrow");
                            let tag_name = element_name(*jsx_el.clone());
                            if tag_name == "cx" {
                                let mut old_expr = arrow_fn_expr.clone();
                                old_expr.body = BlockStmtOrExpr::Expr(Box::new(
                                    self.transform_cx_element(jsx_el),
                                ));
                                *expr = Expr::Call(CallExpr {
                                    span: DUMMY_SP,
                                    callee: Callee::Expr(Box::new(Expr::Ident(Ident {
                                        span: DUMMY_SP,
                                        sym: "createFunctionalComponent".into(),
                                        optional: false,
                                    }))),
                                    args: vec![ExprOrSpread {
                                        expr: Box::new(Expr::Arrow(old_expr)),
                                        spread: None,
                                    }],
                                    type_args: None,
                                });

                                if self.imports.contains_key("cx/ui") {
                                    self.imports
                                        .get_mut("cx/ui")
                                        .unwrap()
                                        .insert("createFunctionalComponent".into());
                                } else {
                                    let mut import_set: HashSet<String> = HashSet::new();
                                    import_set.insert("createFunctionalComponent".into());
                                    self.imports.insert("cx/ui".into(), import_set);
                                }
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }
        expr.visit_mut_children_with(self);

        match expr.borrow() {
            Expr::JSXElement(jsx_el) => {
                let tag_name = element_name(*jsx_el.clone());
                if tag_name == "cx" || tag_name == "Cx" {
                    *expr = self.transform_cx_element(jsx_el.clone());
                }
            }
            _ => {}
        }
    }

    fn visit_mut_import_decl(&mut self, import_decl: &mut ImportDecl) {
        import_decl.visit_mut_children_with(self);
        let src: String = import_decl.src.value.to_string();
        let specifiers_vec: Vec<String> = import_decl
            .specifiers
            .iter_mut()
            .filter(|s| s.is_named())
            .map(|s| match s {
                ImportSpecifier::Named(named) => named.local.sym.to_string(),
                _ => panic!(),
            })
            .collect::<Vec<_>>();

        let specifiers_set: HashSet<String> =
            HashSet::from_iter(specifiers_vec.iter().map(|s| s.clone()));

        let imports_ref = self.imports.get_mut(&src);
        match imports_ref {
            Some(import_set) => import_set.extend(specifiers_set),
            None => {
                self.imports.insert(src, specifiers_set);
            }
        }
    }

    fn visit_mut_module(&mut self, module: &mut Module) {
        module.visit_mut_children_with(self);

        let existing_imports = module
            .body
            .iter_mut()
            .filter(|m| match m {
                ModuleItem::ModuleDecl(module_decl) => match module_decl {
                    ModuleDecl::Import(i) => true,
                    _ => false,
                },
                _ => false,
            })
            .map(|m| match m {
                ModuleItem::ModuleDecl(module_decl) => match module_decl {
                    ModuleDecl::Import(i) => i.clone(),
                    _ => panic!(),
                },
                _ => panic!(),
            })
            .collect::<Vec<_>>();

        let unchanged_module_items = module
            .body
            .iter()
            .filter(|m| match m {
                ModuleItem::ModuleDecl(module_decl) => match module_decl {
                    ModuleDecl::Import(_) => false,
                    _ => true,
                },
                _ => true,
            })
            .cloned()
            .collect::<Vec<_>>();

        let mut new_imports = self
            .imports
            .iter_mut()
            .map(|i| {
                ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                    span: DUMMY_SP,
                    specifiers: i
                        .1
                        .iter()
                        .map(|s| {
                            ImportSpecifier::Named(ImportNamedSpecifier {
                                span: DUMMY_SP,
                                local: Ident {
                                    span: DUMMY_SP,
                                    optional: false,
                                    sym: s.clone().into(),
                                },
                                imported: None,
                                is_type_only: false,
                            })
                        })
                        .collect::<Vec<_>>(),
                    src: Str::from(i.0.to_string()),
                    type_only: false,
                    asserts: None,
                }))
            })
            .collect::<Vec<_>>();

        new_imports.extend(unchanged_module_items);
        module.body = new_imports;
    }
}

// impl VisitMut for ImportVisitor {
//     fn visit_mut_import_decl(&mut self, import_decl: &mut ImportDecl) {
//         import_decl.visit_mut_children_with(self);
//         for i in self.imports.clone() {
//             println!("{}", i);
//         }
//     }
// }

#[derive(Clone)]
pub struct TransformVisitor {
    imports: HashMap<String, HashSet<String>>,
}

pub struct ImportVisitor {
    imports: Vec<String>,
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

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    let imports: HashMap<String, HashSet<String>> = HashMap::new();
    let tv = TransformVisitor { imports };

    let program = program.fold_with(&mut as_folder(tv));

    // program.fold_with(&mut as_folder(ImportVisitor {
    //     imports: imports.clone(),
    // }));

    program
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
        &|_| {
            chain!(
                as_folder(TransformVisitor {
                    imports: HashMap::new()
                }),
                as_folder(TransformVisitor {
                    imports: HashMap::new()
                })
            )
        }, // This works but i do not know how and why
        &input,
        &output,
    )
}
