use std::collections::HashMap;

use indexmap::IndexMap;

use crate::structure::{Error, Expression, Location, Locate, Function, Statement, Type};

struct SymbolTable<'a> {
    functions: HashMap<&'a str, (Type, IndexMap<&'a str, Type>)>,
    tables: Vec<HashMap<&'a str, Type>>,
    current_func: Option<&'a str>,
}

impl<'a> SymbolTable<'a> {
    fn new() -> SymbolTable<'a> {
        SymbolTable {
            functions: HashMap::new(),
            tables: vec![HashMap::new()],
            current_func: None,
        }
    }

    fn enter(&mut self) {
        self.tables.push(HashMap::new());
    }

    fn leave(&mut self) {
        self.tables.pop();
    }

    fn insert(&mut self, name: &'a str, r#type: Type) {
        self.tables.last_mut().unwrap().insert(name, r#type);
    }

    fn get(&self, name: &str) -> Option<Type> {
        self.tables.iter().rev().find_map(|env| env.get(name)).copied()
    }

    fn get_return_type(&self) -> Type {
        let name = self.current_func.unwrap();
        self.functions.get(name).unwrap().0
    }

    fn update_return_type(&mut self, return_type: Type) {
        let name = self.current_func.unwrap();
        let (_, parameters) = self.functions.remove(name).unwrap();
        self.functions.insert(name, (return_type, parameters));
    }

    fn get_parameters(&self) -> IndexMap<&'a str, Type> {
        let name = self.current_func.unwrap();
        self.functions.get(name).unwrap().1.clone()
    }

    fn update_parameters(&mut self, parameters: IndexMap<&'a str, Type>) {
        let name = self.current_func.unwrap();
        let (return_type, _) = self.functions.remove(name).unwrap();
        self.functions.insert(name, (return_type, parameters));
    }
}

pub struct Resolver<'a> {
    generic_ast: Option<Vec<Function<'a>>>,
    errors: Option<Vec<Error>>,
    symbol_table: SymbolTable<'a>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<Function<'a>>, errors: Vec<Error>) -> Resolver {
        Resolver {
            generic_ast: Some(generic_ast),
            errors: Some(errors),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn run(&mut self) -> (Vec<Function<'a>>, Vec<Error>) {
        let mut multi_func_flag = false;
        let ast: Vec<_> = self.generic_ast.take().unwrap().into_iter().map(|func| {
            let name = func.name;
            let return_type = func.r#type;
            let parameters = func.parameters.clone();
            if self.symbol_table.functions.contains_key(name) {
                self.push_error("Multiple function definitions.", func.locate());
                multi_func_flag = true;
            } else {
                self.symbol_table.functions.insert(name, (return_type, parameters));
            }
            func
        }).collect();
        if multi_func_flag {
            return (ast, self.errors.take().unwrap());
        }
        let ast: Vec<_> = ast.into_iter().map(|func| self.resolve_function(func)).collect();
        let ast: Vec<_> = ast.into_iter().map(|mut func| {
            let name = func.name;
            let location = func.locate();
            let (return_type, parameters) = self.symbol_table.functions.remove(name).unwrap();
            match return_type {
                Type::T { .. } => {
                    let message =
                        format!("Unresolved return type of function `{}`.", name);
                    self.push_error(&message, location);
                }
                _ => func.r#type = return_type,
            }
            for (param, r#type) in &parameters {
                match r#type {
                    Type::T { .. } => {
                        let message = format!(
                            "Unresolved parameter `{}` in function `{}`.",
                            param, name
                        );
                        self.push_error(&message, location);
                    }
                    _ => {}
                }
            }
            func.parameters = parameters;
            func
        }).collect();
        (ast, self.errors.take().unwrap())
    }

    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.as_mut().unwrap().push(Error::Resolving {
            message: message.to_string(),
            location,
        });
    }

    // fn unify_types(&self, r#typel: Type, r#typer: Type) -> Result<(Type, Type), ()> {
    //     match r#typel {
    //         Type::T(_) => Ok((r#typer, r#typer)),
    //         Type::Void(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Void(_) => Ok((r#typel, r#typer)),
    //             _ => Err(()),
    //         },
    //         Type::Char(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_) => Ok((r#typel, r#typer)),
    //             _ => Err(()),
    //         },
    //         Type::Short(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_) | Type::Short(_) => Ok((r#typel, r#typer)),
    //             _ => Err(()),
    //         },
    //         Type::Int(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_) | Type::Short(_) | Type::Int(_) => Ok((r#typel, r#typer)),
    //             _ => Err(()),
    //         },
    //         Type::Long(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_) | Type::Short(_) | Type::Int(_) | Type::Long(_) => {
    //                 Ok((r#typel, r#typer))
    //             }
    //             _ => Err(()),
    //         },
    //         Type::Float(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_) | Type::Short(_) | Type::Int(_) | Type::Long(_) | Type::Float(_) => {
    //                 Ok((r#typel, r#typer))
    //             }
    //             _ => Err(()),
    //         },
    //         Type::Double(_) => match r#typer {
    //             Type::T(_) => Ok((r#typel, r#typel)),
    //             Type::Char(_)
    //             | Type::Short(_)
    //             | Type::Int(_)
    //             | Type::Long(_)
    //             | Type::Float(_)
    //             | Type::Double(_) => Ok((r#typel, r#typer)),
    //             _ => Err(()),
    //         },
    //         Type::Signed(_) => panic!("Type::Signed not implemented."),
    //         Type::Unsigned(_) => panic!("Type::Unsigned not implemented."),
    //     }
    // }

    fn resolve_function(&mut self, function: Function<'a>) -> Function<'a> {
        function
        // let Function {
        //     r#type,
        //     name,
        //     parameters,
        //     body,
        //     location,
        // } = func;
        // self.symbol_table.current_func = name;
        // self.symbol_table.enter();
        // for (param, r#type) in parameters.iter() {
        //     self.symbol_table.insert(param, *r#type);
        // }
        // let body = self.resolve_statement(body);
        // let mut new_parameters = IndexMap::new();
        // for param in parameters.keys() {
        //     let r#type = self.symbol_table.get(param).unwrap();
        //     new_parameters.insert(*param, r#type);
        // }
        // self.symbol_table.update_parameters(new_parameters);
        // self.symbol_table.leave();
        // Function {
        //     r#type,
        //     name,
        //     parameters,
        //     body,
        //     location,
        // }
    }

    //     fn resolve_statement(&mut self, stmt: Statement<'a>) -> Statement<'a> {
    //         match stmt {
    //             st @ Statement::Continue(_) => st,
    //             st @ Statement::Break(_) => st,
    //             Statement::Expr(expr) => {
    //                 let (_, expr) = self.resolve_expression(expr);
    //                 Statement::Expr(expr)
    //             }
    //             Statement::Return { expr, location } => {
    //                 let (r#type, expr) = match expr {
    //                     Some(expr) => {
    //                         let (r#type, expr) = self.resolve_expression(expr);
    //                         (r#type, Some(expr))
    //                     }
    //                     None => (Type::Void(Location::empty()), None),
    //                 };
    //                 let return_type = self.symbol_table.get_return_type();
    //                 match self.unify_types(return_type, r#type) {
    //                     Ok((return_type, _)) => self.symbol_table.update_return_type(return_type),
    //                     Err(_) => {
    //                         let message = format!(
    //                             "`{:?}` is expected to return `{:?}`, get `{:?}`.",
    //                             self.symbol_table.current_func, return_type, r#type
    //                         );
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 Statement::Return { expr, location }
    //             }
    //             Statement::Block {
    //                 statements,
    //                 location,
    //             } => {
    //                 let mut sts = Vec::new();
    //                 self.symbol_table.enter();
    //                 for st in statements.into_iter() {
    //                     sts.push(Box::new(self.resolve_statement(*st)));
    //                 }
    //                 self.symbol_table.leave();
    //                 Statement::Block {
    //                     statements: sts,
    //                     location,
    //                 }
    //             }
    //             Statement::Def {
    //                 r#type,
    //                 declarators,
    //                 location,
    //             } => {
    //                 let mut decls = Vec::new();
    //                 for (ident, init) in declarators.into_iter() {
    //                     let (typ, expr) = match init {
    //                         Some(expr) => {
    //                             let (t, e) = self.resolve_expression(expr);
    //                             (t, Some(e))
    //                         }
    //                         None => (Type::T(Location::empty()), None),
    //                     };
    //                     match self.unify_types(r#type, typ) {
    //                         Ok(_) => {}
    //                         Err(_) => {
    //                             let message = format!(
    //                                 "Expect `{:?}` to be `{:?}`, get `{:?}`.",
    //                                 ident, r#type, typ
    //                             );
    //                             self.push_error(&message, location);
    //                         }
    //                     }
    //                     self.symbol_table.insert(ident, r#type);
    //                     decls.push((ident, expr));
    //                 }
    //                 Statement::Def {
    //                     r#type,
    //                     declarators: decls,
    //                     location,
    //                 }
    //             }
    //             Statement::While {
    //                 condition,
    //                 body,
    //                 location,
    //             } => {
    //                 let (r#type, condition) = self.resolve_expression(condition);
    //                 match self.unify_types(Type::Int(Location::empty()), r#type) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message =
    //                             format!("Expect loop condition to be `Int`, get `{:?}`.", r#type);
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 let body = Box::new(self.resolve_statement(*body));
    //                 Statement::While {
    //                     condition,
    //                     body,
    //                     location,
    //                 }
    //             }
    //             Statement::Do {
    //                 condition,
    //                 body,
    //                 location,
    //             } => {
    //                 let (r#type, condition) = self.resolve_expression(condition);
    //                 match self.unify_types(Type::Int(Location::empty()), r#type) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message =
    //                             format!("Expect loop condition to be `Int`, get `{:?}`.", r#type);
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 let body = Box::new(self.resolve_statement(*body));
    //                 Statement::Do {
    //                     condition,
    //                     body,
    //                     location,
    //                 }
    //             }
    //             Statement::For {
    //                 initialization,
    //                 condition,
    //                 increment,
    //                 body,
    //                 location,
    //             } => {
    //                 let initialization = match initialization {
    //                     Some(expr) => Some(self.resolve_expression(expr).1),
    //                     None => None,
    //                 };
    //                 let condition = match condition {
    //                     Some(expr) => Some(self.resolve_expression(expr).1),
    //                     None => None,
    //                 };
    //                 let increment = match increment {
    //                     Some(expr) => Some(self.resolve_expression(expr).1),
    //                     None => None,
    //                 };
    //                 let body = Box::new(self.resolve_statement(*body));
    //                 Statement::For {
    //                     initialization,
    //                     condition,
    //                     increment,
    //                     body,
    //                     location,
    //                 }
    //             }
    //             Statement::If {
    //                 condition,
    //                 body,
    //                 alternative,
    //                 location,
    //             } => {
    //                 let (r#type, condition) = self.resolve_expression(condition);
    //                 match self.unify_types(Type::Int(Location::empty()), r#type) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message =
    //                             format!("Expect loop condition to be `Int`, get `{:?}`.", r#type);
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 let body = Box::new(self.resolve_statement(*body));
    //                 let alternative = match alternative {
    //                     Some(stmt) => Some(Box::new(self.resolve_statement(*stmt))),
    //                     None => None,
    //                 };
    //                 Statement::If {
    //                     condition,
    //                     body,
    //                     alternative,
    //                     location,
    //                 }
    //             }
    //             Statement::Switch {
    //                 expression,
    //                 branches,
    //                 default,
    //                 location,
    //             } => {
    //                 let (_, expression) = self.resolve_expression(expression);
    //                 let mut branches_ = Vec::new();
    //                 for (label, stmts) in branches.into_iter() {
    //                     let label_ = self.resolve_expression(label).1;
    //                     let mut stmts_ = Vec::new();
    //                     for stmt in stmts.into_iter() {
    //                         stmts_.push(Box::new(self.resolve_statement(*stmt)));
    //                     }
    //                     branches_.push((label_, stmts_));
    //                 }
    //                 let default = match default {
    //                     Some(stmts) => {
    //                         let mut stmts_ = Vec::new();
    //                         for stmt in stmts.into_iter() {
    //                             stmts_.push(Box::new(self.resolve_statement(*stmt)));
    //                         }
    //                         Some(stmts_)
    //                     }
    //                     None => None,
    //                 };
    //                 Statement::Switch {
    //                     expression,
    //                     branches: branches_,
    //                     default,
    //                     location,
    //                 }
    //             }
    //         }
    //     }

    //     fn resolve_expression(&mut self, expr: Expression<'a>) -> (Type, Expression<'a>) {
    //         match expr {
    //             Expression::Ident { value, location } => match self.symbol_table.get(value) {
    //                 Some(r#type) => (r#type, Expression::Ident { value, location }),
    //                 None => {
    //                     let message = format!("Undefined ident `{:?}`.", value);
    //                     self.push_error(&message, location);
    //                     (
    //                         Type::T(Location::empty()),
    //                         Expression::Ident { value, location },
    //                     )
    //                 }
    //             },
    //             ex @ Expression::IntConst { .. } => (Type::Int(Location::empty()), ex),
    //             ex @ Expression::FloatingConst { .. } => (Type::Float(Location::empty()), ex),
    //             ex @ Expression::CharConst { .. } => (Type::Char(Location::empty()), ex),
    //             Expression::StrConst { .. } => panic!("Resolver for StrConst is not implemented."),
    //             Expression::Prefix {
    //                 operator,
    //                 expression,
    //                 location,
    //             } => {
    //                 let (r#type, expression) = self.resolve_expression(*expression);
    //                 match self.unify_types(Type::Double(Location::empty()), r#type) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message = format!(
    //                             "Expect numbers after `prefix {:?}`, get `{:?}`.",
    //                             operator, r#type
    //                         );
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 (
    //                     r#type,
    //                     Expression::Prefix {
    //                         operator,
    //                         expression: Box::new(expression),
    //                         location,
    //                     },
    //                 )
    //             }
    //             Expression::Infix {
    //                 left,
    //                 operator,
    //                 right,
    //             } => {
    //                 let location = left.locate();
    //                 let (r#typel, expr_l) = self.resolve_expression(*left);
    //                 let (r#typer, expr_r) = self.resolve_expression(*right);
    //                 match self.unify_types(Type::Double(Location::empty()), r#typel) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message = format!(
    //                             "Expect numbers before `infix {:?}`, get `{:?}`.",
    //                             operator, r#typel
    //                         );
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 match self.unify_types(Type::Double(Location::empty()), r#typer) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message = format!(
    //                             "Expect numbers after `infix {:?}`, get `{:?}`.",
    //                             operator, r#typer
    //                         );
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 (
    //                     r#typer,
    //                     Expression::Infix {
    //                         left: Box::new(expr_l),
    //                         operator,
    //                         right: Box::new(expr_r),
    //                     },
    //                 )
    //             }
    //             Expression::Suffix {
    //                 operator,
    //                 expression,
    //             } => {
    //                 let location = expression.locate();
    //                 let (r#type, expression) = self.resolve_expression(*expression);
    //                 match self.unify_types(Type::Double(Location::empty()), r#type) {
    //                     Ok(_) => {}
    //                     Err(_) => {
    //                         let message = format!(
    //                             "Expect numbers before `suffix {:?}`, get `{:?}`.",
    //                             operator, r#type
    //                         );
    //                         self.push_error(&message, location);
    //                     }
    //                 }
    //                 (
    //                     r#type,
    //                     Expression::Suffix {
    //                         operator,
    //                         expression: Box::new(expression),
    //                     },
    //                 )
    //             }
    //             Expression::Index { .. } => {
    //                 panic!("Expression::Index not implemented!");
    //             }
    //             Expression::Call { name, arguments } => {
    //                 let (name, location) = match *name {
    //                     Expression::Ident { value, location } => (value, location),
    //                     _ => panic!("Call only with str const"),
    //                 };
    //                 let (return_type, parameters): (Type, Vec<Type>) =
    //                     match self.symbol_table.functions.get(name) {
    //                         None => {
    //                             let message = format!("Undefined function `{:?}`.", name);
    //                             self.push_error(&message, location);
    //                             return (
    //                                 Type::T(Location::empty()),
    //                                 Expression::Call {
    //                                     name: Box::new(Expression::Ident {
    //                                         value: name,
    //                                         location,
    //                                     }),
    //                                     arguments,
    //                                 },
    //                             );
    //                         }
    //                         Some((return_type, parameters)) => {
    //                             let mut parameters_ = Vec::new();
    //                             for param in parameters.values() {
    //                                 parameters_.push(*param);
    //                             }
    //                             (*return_type, parameters_)
    //                         }
    //                     };
    //                 let mut arguments_ = Vec::new();
    //                 for (arg, r#typeparam) in arguments.into_iter().zip(parameters.iter()) {
    //                     let (r#typearg, arg) = self.resolve_expression(*arg);
    //                     match self.unify_types(*r#typeparam, r#typearg) {
    //                         Ok(_) => {}
    //                         Err(_) => {
    //                             let message = format!("Unmatched arguments in `{:?}`.", name);
    //                             self.push_error(&message, location);
    //                         }
    //                     }
    //                     arguments_.push(Box::new(arg));
    //                 }
    //                 (
    //                     return_type,
    //                     Expression::Call {
    //                         name: Box::new(Expression::Ident {
    //                             value: name,
    //                             location,
    //                         }),
    //                         arguments: arguments_,
    //                     },
    //                 )
    //             }
    //         }
    //     }
}
