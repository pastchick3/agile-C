use std::collections::HashMap;

use indexmap::IndexMap;

use crate::structure::{ Location, Locate, Error, Type, Expression, Statement, Function };

struct SymbolTable<'a> {
    functions: HashMap<&'a str, (Type, IndexMap<&'a str, Type>)>,
    tables: Vec<HashMap<&'a str, Type>>,
    current_func: &'a str,
}

impl<'a> SymbolTable<'a> {
    fn new() -> SymbolTable<'a> {
        SymbolTable {
            functions: HashMap::new(),
            tables: vec![HashMap::new()],
            current_func: "global",
        }
    }

    fn enter(&mut self) {
        self.tables.push(HashMap::new());
    }

    fn leave(&mut self) {
        self.tables.pop();
    }

    fn insert(&mut self, name: &'a str, type_: Type) {
        self.tables.last_mut().unwrap().insert(name, type_);
    }

    fn get(&self, name: &str) -> Option<Type> {
        for index in (0..self.tables.len()).rev() {
            match self.tables[index].get(name) {
                Some(type_) => return Some(*type_),
                None => {},
            }
        }
        None
    }

    fn get_return_type(&self) -> Type {
        let (return_type, _) = self.functions.get(self.current_func).unwrap();
        *return_type
    }

    fn get_parameters(&self) -> &IndexMap<&'a str, Type> {
        let (_, parameters) = self.functions.get(self.current_func).unwrap();
        parameters
    }

    fn update_return_type(&mut self, return_type: Type) {
        let (_, parameters) = self.functions.remove(self.current_func).unwrap();
        self.functions.insert(self.current_func, (return_type, parameters));
    }

    fn update_parameters(&mut self, parameters: IndexMap<&'a str, Type>) {
        let (return_type, _) = self.functions.remove(self.current_func).unwrap();
        self.functions.insert(self.current_func, (return_type, parameters));
    }
}

pub struct Resolver<'a> {
    generic_ast: Vec<Function<'a>>,
    inter_ast: Vec<Function<'a>>,
    symbol_table: SymbolTable<'a>,
    errors: Option<Vec<Error>>,
    ast: Option<Vec<Function<'a>>>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<Function<'a>>, errors: Vec<Error>) -> Resolver {
        Resolver {
            generic_ast,
            inter_ast: Vec::new(),
            symbol_table: SymbolTable::new(),
            errors: Some(errors),
            ast: Some(Vec::new()),
        }
    }

    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.as_mut().unwrap().push(
            Error::Resolving {
                message: message.to_string(),
                location,
            }
        );
    }

    fn unify_types(&self, type_l: Type, type_r: Type) -> Result<(Type, Type), ()> {
        match type_l {
            Type::T(_) => Ok((type_r, type_r)),
            Type::Void(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Void(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Char(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Short(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_) | Type::Short(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Int(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_)
                | Type::Short(_)
                | Type::Int(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Long(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_)
                | Type::Short(_)
                | Type::Int(_)
                | Type::Long(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Float(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_)
                | Type::Short(_)
                | Type::Int(_)
                | Type::Long(_)
                | Type::Float(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Double(_) => match type_r {
                Type::T(_) => Ok((type_l, type_l)),
                Type::Char(_)
                | Type::Short(_)
                | Type::Int(_)
                | Type::Long(_)
                | Type::Float(_)
                | Type::Double(_) => Ok((type_l, type_r)),
                _ => Err(()),
            },
            Type::Signed(_) => panic!("Type::Signed not implemented."),
            Type::Unsigned(_) => panic!("Type::Unsigned not implemented."),
        }
    }

    pub fn run(&mut self) -> (Vec<Function<'a>>, Vec<Error>) {
        for func in self.generic_ast.iter() {
            let name = func.name;
            let return_type = func.type_;
            let parameters = func.parameters.clone();
            self.symbol_table.functions.insert(name, (return_type, parameters));
        }
        loop {
            match self.generic_ast.pop() {
                Some(func) => {
                    let func = self.resolve_function(func);
                    self.inter_ast.push(func);
                },
                None => break,
            }
        }
        loop {
            match self.inter_ast.pop() {
                Some(mut func) => {
                    let name = func.name;
                    let (return_type, parameters) = self.symbol_table.functions.remove(name).unwrap();
                    match return_type {
                        Type::T(_) => {
                            let message = format!("Unresolved return type of function `{:?}`.", name);
                            self.push_error(&message, func.location);
                        },
                        _ => { func.type_ = return_type },
                    }
                    for (param, type_) in parameters.iter() {
                        match type_ {
                            Type::T(_) => {
                                let message = format!("Unresolved parameter `{:?}` in function `{:?}`.", param, name);
                                self.push_error(&message, func.location);
                            },
                            _ => {},
                        }
                    }
                    func.parameters = parameters.clone();
                    self.ast.as_mut().unwrap().push(func);
                },
                None => break,
            }
        }
        (self.ast.take().unwrap(), self.errors.take().unwrap())
    }

    fn resolve_function(&mut self, func: Function<'a>) -> Function<'a> {
        let Function { type_, name, parameters, body, location } = func;
        self.symbol_table.current_func = name;
        let body = self.resolve_statement(body);
        Function { type_, name, parameters, body, location }
    }

    fn resolve_statement(&mut self, stmt: Statement<'a>) -> Statement<'a> {
        match stmt {
            st @ Statement::Continue(_) => st,
            st @ Statement::Break(_) => st,
            Statement::Expr(expr) => {
                let (types, expr) = self.resolve_expression(expr);
                Statement::Expr(expr)
            },
            Statement::Return { expr, location } => {
                let (type_, expr) = match expr {
                    Some(expr) => {
                        let (type_, expr) = self.resolve_expression(expr);
                        (type_, Some(expr))
                    },
                    None => (Type::Void(Location::empty()), None),
                };
                let return_type = self.symbol_table.get_return_type();
                match self.unify_types(return_type, type_) {
                    Ok((return_type, type_)) => self.symbol_table.update_return_type(return_type),
                    Err(_) => {
                        let message = format!("`{:?}` is expected to return `{:?}`, get `{:?}`.",
                                        self.symbol_table.current_func, return_type, type_);
                        self.push_error(&message, location);
                    },
                }
                Statement::Return { expr, location }
            },
        //     Statement::Block { statements, location: _ } => {
        //         self.push_str_newline("{");
        //         self.ident_level += 1;
        //         if statements.is_empty() {
        //             self.pop_char();
        //         } else {
        //             for st in statements.into_iter() {
        //                 self.serialize_statement(*st);
        //             }
        //         }
        //         self.ident_level -= 1;
        //         self.push_str(&" ".repeat(self.ident_level*4));
        //         self.push_str_newline("}");
        //     },
        //     Statement::Def { types, declarators, location: _ } => {
        //         self.serialize_types(types);
        //         for (ident, init) in declarators.into_iter() {
        //             self.serialize_expression(ident);
        //             match init {
        //                 Some(ex) => {
        //                     self.push_str_space("=");
        //                     self.serialize_expression(ex);
        //                 },
        //                 None => {},
        //             }
        //             self.pop_char();
        //             self.push_str_space(",");
        //         }
        //         self.pop_char();
        //         self.pop_char();
        //         self.push_str_newline(";")
        //     },
        //     Statement::While { condition, body, location: _ } => {
        //         self.push_str_space("while");
        //         self.push_str("(");
        //         self.serialize_expression(condition);
        //         self.pop_char();
        //         self.push_str_space(")");
        //         self.serialize_statement(*body);
        //     },
        //     Statement::Do { condition, body, location: _ } => {
        //         self.push_str_space("do");
        //         self.serialize_statement(*body);
        //         self.pop_char();
        //         self.push_str(" ");
        //         self.push_str_space("while");
        //         self.push_str("(");
        //         self.serialize_expression(condition);
        //         self.pop_char();
        //         self.push_str(")");
        //         self.push_str_newline(";");
        //     },
        //     Statement::For { initialization, condition, increment, body, location: _ } => {
        //         self.push_str_space("for");
        //         self.push_str("(");
        //         match initialization {
        //             Some(ex) => {
        //                 self.serialize_expression(ex);
        //                 self.pop_char();
        //             },
        //             None => self.push_str_space(""),
        //         }
        //         self.push_str_space(";");
        //         match condition {
        //             Some(ex) => {
        //                 self.serialize_expression(ex);
        //                 self.pop_char();
        //             },
        //             None => {},
        //         }
        //         self.push_str_space(";");
        //         match increment {
        //             Some(ex) => {
        //                 self.serialize_expression(ex);
        //                 self.pop_char();
        //             },
        //             None => {},
        //         }
        //         self.push_str_space(")");
        //         self.serialize_statement(*body);
        //     },
        //     Statement::If { condition, body, alternative, location: _ } => {
        //         self.push_str_space("if");
        //         self.push_str("(");
        //         self.serialize_expression(condition);
        //         self.pop_char();
        //         self.push_str_space(")");
        //         self.serialize_statement(*body);
        //         match alternative {
        //             Some(st) => {
        //                 self.pop_char();
        //                 self.push_str_space(" else");
        //                 self.serialize_statement(*st);
        //             },
        //             None => {},
        //         }
        //     },
        //     Statement::Switch { expression, branches, default, location: _ } => {
        //         self.push_str_space("switch");
        //         self.push_str("(");
        //         self.serialize_expression(expression);
        //         self.pop_char();
        //         self.push_str_space(")");
        //         self.push_str_newline("{");
        //         self.ident_level += 1;
        //         for (label, sts) in branches.into_iter() {
        //             self.push_str(&" ".repeat(self.ident_level*4));
        //             self.push_str_space("case");
        //             self.serialize_expression(label);
        //             self.pop_char();
        //             self.push_str_newline(":");
        //             self.ident_level += 1;
        //             for st in sts.into_iter() {
        //                 self.serialize_statement(*st);
        //             }
        //             self.ident_level -= 1;
        //         }
        //         match default {
        //             Some(sts) => {
        //                 self.push_str(&" ".repeat(self.ident_level*4));
        //                 self.push_str_newline("default:");
        //                 self.ident_level += 1;
        //                 for st in sts.into_iter() {
        //                     self.serialize_statement(*st);
        //                 }
        //                 self.ident_level -= 1;
        //             },
        //             None => {},
        //         }
        //         self.ident_level -= 1;
        //         self.push_str(&" ".repeat(self.ident_level*4));
        //         self.push_str_newline("}");
        //     },
            _ => panic!("STMT"),
        }
    }

    fn resolve_expression(&mut self, expr: Expression<'a>) -> (Type, Expression<'a>) {
        match expr {
            // Expression::Ident { value, location: _ } => self.push_str_space(value),
            ex @ Expression::IntConst { .. } => (Type::Int(Location::empty()), ex),
            // Expression::FloatingConst { value, location: _ } => self.push_str_space(&value.to_string()),
            // Expression::CharConst { value, location: _ } => self.push_str_space(value),
            // Expression::StrConst { value, location: _ } => self.push_str_space(value),
            // Expression::Prefix { operator, expression, location: _ } => {
            //     self.push_str(operator);
            //     self.serialize_expression(*expression);
            // },
            // Expression::Infix { left, operator, right } => {
            //     self.serialize_expression(*left);
            //     self.push_str_space(operator);
            //     self.serialize_expression(*right);
            // },
            // Expression::Suffix { operator, expression } => {
            //     self.serialize_expression(*expression);
            //     self.pop_char();
            //     self.push_str_space(operator);
            // },
            Expression::Index { .. } => {
                panic!("Expression::Index not implemented!");
            },
            // Expression::Call { name, arguments } => {
            //     self.serialize_expression(*name);
            //     self.pop_char();
            //     self.push_str("(");
            //     if !arguments.is_empty() {
            //         for arg in arguments.into_iter() {
            //             self.serialize_expression(*arg);
            //             self.pop_char();
            //             self.push_str_space(",");
            //         }
            //         self.pop_char();
            //         self.pop_char();
            //     }
            //     self.push_str_space(")");
            // },
            _ => panic!("EXPR"),
        }
    }
}
