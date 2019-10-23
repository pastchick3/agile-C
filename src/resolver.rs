use std::collections::HashMap;

use indexmap::IndexMap;

use crate::structure::{Array, Error, Expression, Function, Locate, Location, Statement, Type};

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
        self.tables
            .iter()
            .rev()
            .find_map(|env| env.get(name))
            .copied()
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
        let ast: Vec<_> = self
            .generic_ast
            .take()
            .unwrap()
            .into_iter()
            .map(|func| {
                let name = func.name;
                let return_type = func.r#type;
                let parameters = func.parameters.clone();
                if self.symbol_table.functions.contains_key(name) {
                    self.push_error("Multiple function definitions.", func.locate());
                    multi_func_flag = true;
                } else {
                    self.symbol_table
                        .functions
                        .insert(name, (return_type, parameters));
                }
                func
            })
            .collect();
        if multi_func_flag {
            return (ast, self.errors.take().unwrap());
        }
        let ast: Vec<_> = ast
            .into_iter()
            .map(|func| self.resolve_function(func))
            .collect();
        let ast: Vec<_> = ast
            .into_iter()
            .map(|mut func| {
                let name = func.name;
                let location = func.locate();
                let (return_type, parameters) = self.symbol_table.functions.remove(name).unwrap();
                match return_type {
                    Type::T { .. } => {
                        let message = format!("Unresolved return type of function `{}`.", name);
                        self.push_error(&message, location);
                    }
                    _ => func.r#type = return_type,
                }
                for (param, r#type) in &parameters {
                    match r#type {
                        Type::T { .. } => {
                            let message =
                                format!("Unresolved parameter `{}` in function `{}`.", param, name);
                            self.push_error(&message, location);
                        }
                        _ => {}
                    }
                }
                func.parameters = parameters;
                func
            })
            .collect();
        (ast, self.errors.take().unwrap())
    }

    fn push_error(&mut self, message: &str, location: Location) {
        self.errors.as_mut().unwrap().push(Error::Resolving {
            message: message.to_string(),
            location,
        });
    }

    fn unify_types(&self, type_left: Type, type_right: Type) -> Result<(Type, Type), ()> {
        use Type::*;

        match type_left {
            T { .. } => Ok((type_right, type_right)),
            Void { .. } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Void { .. } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Char { .. } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Short {
                signed_flag: true, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short {
                    signed_flag: true, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Short {
                signed_flag: false, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short {
                    signed_flag: false, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Int {
                signed_flag: true, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short { .. }
                | Int {
                    signed_flag: true, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Int {
                signed_flag: false, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short {
                    signed_flag: false, ..
                }
                | Int {
                    signed_flag: false, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Long {
                signed_flag: true, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short { .. }
                | Int { .. }
                | Long {
                    signed_flag: true, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Long {
                signed_flag: false, ..
            } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short {
                    signed_flag: false, ..
                }
                | Int {
                    signed_flag: false, ..
                }
                | Long {
                    signed_flag: false, ..
                } => Ok((type_left, type_right)),
                _ => Err(()),
            },
            Float { .. } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. } | Short { .. } | Int { .. } | Long { .. } | Float { .. } => {
                    Ok((type_left, type_right))
                }
                _ => Err(()),
            },
            Double { .. } => match type_right {
                T { .. } => Ok((type_left, type_left)),
                Char { .. }
                | Short { .. }
                | Int { .. }
                | Long { .. }
                | Float { .. }
                | Double { .. } => Ok((type_left, type_right)),
                _ => Err(()),
            },
        }
    }

    fn resolve_function(&mut self, function: Function<'a>) -> Function<'a> {
        let Function {
            r#type,
            name,
            parameters,
            body,
            location,
        } = function;
        self.symbol_table.current_func = Some(name);
        self.symbol_table.enter();
        for (param, r#type) in &parameters {
            self.symbol_table.insert(param, *r#type);
        }
        let body = self.resolve_statement(body);
        let mut new_parameters = IndexMap::new();
        for param in parameters.keys() {
            let r#type = self.symbol_table.get(param).unwrap();
            new_parameters.insert(*param, r#type);
        }
        self.symbol_table.update_parameters(new_parameters.clone());
        self.symbol_table.leave();
        self.symbol_table.current_func = None;
        Function {
            r#type,
            name,
            parameters: new_parameters,
            body,
            location,
        }
    }

    fn resolve_statement(&mut self, statement: Statement<'a>) -> Statement<'a> {
        let dummy_int_signed = Type::Int {
            signed_flag: true,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        match statement {
            stmt @ Statement::Continue(_) => stmt,
            stmt @ Statement::Break(_) => stmt,
            Statement::Expr(expr) => {
                let (_, expr) = self.resolve_expression(expr);
                Statement::Expr(expr)
            }
            Statement::Return {
                expression,
                location,
            } => {
                let (type_right, expression) = match expression {
                    Some(expr) => {
                        let (r#type, expr) = self.resolve_expression(expr);
                        (r#type, Some(expr))
                    }
                    None => (
                        Type::Void {
                            array_flag: false,
                            array_len: None,
                            location: Location::empty(),
                        },
                        None,
                    ),
                };
                let type_left = self.symbol_table.get_return_type();
                match self.unify_types(type_left, type_right) {
                    Ok((return_type, _)) => self.symbol_table.update_return_type(return_type),
                    Err(_) => {
                        let message = format!(
                            "`{}` is expected to return `{}`, get `{}`.",
                            self.symbol_table.current_func.unwrap(),
                            type_left,
                            type_left
                        );
                        self.push_error(&message, location);
                    }
                }
                Statement::Return {
                    expression,
                    location,
                }
            }
            Statement::Block {
                statements,
                location,
            } => {
                self.symbol_table.enter();
                let statements: Vec<_> = statements
                    .into_iter()
                    .map(|stmt| Box::new(self.resolve_statement(*stmt)))
                    .collect();
                self.symbol_table.leave();
                Statement::Block {
                    statements,
                    location,
                }
            }
            Statement::Def {
                declarators,
                location,
            } => {
                let declarators: Vec<_> = declarators
                    .into_iter()
                    .map(|(mut r#type, ident, init)| {
                        let (typ, expr) = match init {
                            Some(expr) => {
                                let (t, e) = self.resolve_expression(expr);
                                (t, Some(e))
                            }
                            None => (
                                Type::T {
                                    dummy_flag: true,
                                    array_flag: false,
                                    array_len: None,
                                    location: Location::empty(),
                                },
                                None,
                            ),
                        };
                        match self.unify_types(r#type, typ) {
                            Ok((typ, _)) => r#type = typ,
                            Err(_) => {
                                let message =
                                    format!("Expect `{}` to be `{}`, get `{}`.", ident, r#type, typ);
                                self.push_error(&message, location);
                            }
                        }
                        self.symbol_table.insert(ident, r#type);
                        (r#type, ident, expr)
                    })
                    .collect();
                Statement::Def {
                    declarators,
                    location,
                }
            }
            Statement::While {
                condition,
                body,
                location,
            } => {
                let (r#type, condition) = self.resolve_expression(condition);
                if let Err(_) = self.unify_types(dummy_int_signed, r#type) {
                    let message = format!("Expect `{}` as the loop condition, get `{}`.", dummy_int_signed, r#type);
                    self.push_error(&message, location);
                }
                let body = Box::new(self.resolve_statement(*body));
                Statement::While {
                    condition,
                    body,
                    location,
                }
            }
            Statement::Do {
                condition,
                body,
                location,
            } => {
                let (r#type, condition) = self.resolve_expression(condition);
                if let Err(_) = self.unify_types(dummy_int_signed, r#type) {
                    let message = format!("Expect `{}` as the loop condition, get `{}`.", dummy_int_signed, r#type);
                    self.push_error(&message, location);
                }
                let body = Box::new(self.resolve_statement(*body));
                Statement::Do {
                    condition,
                    body,
                    location,
                }
            }
            Statement::For {
                initialization,
                condition,
                increment,
                body,
                location,
            } => {
                let initialization = match initialization {
                    Some(expr) => Some(self.resolve_expression(expr).1),
                    None => None,
                };
                let condition = match condition {
                    Some(expr) => Some(self.resolve_expression(expr).1),
                    None => None,
                };
                let increment = match increment {
                    Some(expr) => Some(self.resolve_expression(expr).1),
                    None => None,
                };
                let body = Box::new(self.resolve_statement(*body));
                Statement::For {
                    initialization,
                    condition,
                    increment,
                    body,
                    location,
                }
            }
            Statement::If {
                condition,
                body,
                alternative,
                location,
            } => {
                let (r#type, condition) = self.resolve_expression(condition);
                if let Err(_) = self.unify_types(dummy_int_signed, r#type) {
                    let message = format!("Expect `{}`, get `{}`.", dummy_int_signed, r#type);
                    self.push_error(&message, location);
                }
                let body = Box::new(self.resolve_statement(*body));
                let alternative = match alternative {
                    Some(stmt) => Some(Box::new(self.resolve_statement(*stmt))),
                    None => None,
                };
                Statement::If {
                    condition,
                    body,
                    alternative,
                    location,
                }
            }
            Statement::Switch {
                expression,
                branches,
                default,
                location,
            } => {
                let (_, expression) = self.resolve_expression(expression);
                let branches: Vec<_> = branches
                    .into_iter()
                    .map(|(label, stmts)| {
                        let label = self.resolve_expression(label).1;
                        let stmts: Vec<_> = stmts
                            .into_iter()
                            .map(|stmt| Box::new(self.resolve_statement(*stmt)))
                            .collect();
                        (label, stmts)
                    })
                    .collect();
                let default = match default {
                    Some(stmts) => {
                        let stmts: Vec<_> = stmts
                            .into_iter()
                            .map(|stmt| Box::new(self.resolve_statement(*stmt)))
                            .collect();
                        Some(stmts)
                    }
                    None => None,
                };
                Statement::Switch {
                    expression,
                    branches,
                    default,
                    location,
                }
            }
        }
    }

    fn resolve_expression(&mut self, expression: Expression<'a>) -> (Type, Expression<'a>) {
        let dummy_t = Type::T {
            dummy_flag: true,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_char = Type::Char {
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_short_signed = Type::Short {
            signed_flag: true,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_short_unsigned = Type::Short {
            signed_flag: false,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_int_signed = Type::Int {
            signed_flag: true,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_int_unsigned = Type::Int {
            signed_flag: false,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_long_signed = Type::Long {
            signed_flag: true,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_long_unsigned = Type::Long {
            signed_flag: false,
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_float = Type::Float {
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let dummy_double = Type::Double {
            array_flag: false,
            array_len: None,
            location: Location::empty(),
        };
        let location = expression.locate();
        match expression {
            Expression::Ident { value, location } => match self.symbol_table.get(value) {
                Some(r#type) => (r#type, Expression::Ident { value, location }),
                None => {
                    let message = format!("Undefined ident `{}`.", value);
                    self.push_error(&message, location);
                    (dummy_t, Expression::Ident { value, location })
                }
            },
            Expression::IntConst { value, location } => {
                let str_value = format!("{}", value);
                if let Ok(_) = str_value.parse::<i16>() {
                    return (dummy_short_signed, Expression::IntConst { value, location });
                }
                if let Ok(_) = str_value.parse::<u16>() {
                    return (
                        dummy_short_unsigned,
                        Expression::IntConst { value, location },
                    );
                }
                if let Ok(_) = str_value.parse::<i32>() {
                    return (dummy_int_signed, Expression::IntConst { value, location });
                }
                if let Ok(_) = str_value.parse::<u32>() {
                    return (dummy_int_unsigned, Expression::IntConst { value, location });
                }
                if let Ok(_) = str_value.parse::<i64>() {
                    return (dummy_long_signed, Expression::IntConst { value, location });
                }
                if let Ok(_) = str_value.parse::<u64>() {
                    return (
                        dummy_long_unsigned,
                        Expression::IntConst { value, location },
                    );
                }
                let message = format!("Range error for `{}`.", value);
                self.push_error(&message, location);
                (dummy_t, Expression::IntConst { value, location })
            }
            Expression::FloatConst { value, location } => {
                let str_value = format!("{}", value);
                if let Ok(_) = str_value.parse::<f32>() {
                    return (dummy_float, Expression::FloatConst { value, location });
                }
                if let Ok(_) = str_value.parse::<f64>() {
                    return (dummy_double, Expression::FloatConst { value, location });
                }
                let message = format!("Range error for `{}`.", value);
                self.push_error(&message, location);
                (dummy_t, Expression::FloatConst { value, location })
            }
            Expression::CharConst { value, location } => {
                (dummy_char, Expression::CharConst { value, location })
            }
            Expression::StrConst { value, location } => {
                // self.push_error("Str (char *) has not been implemented.", location);
                (dummy_t, Expression::StrConst { value, location })
            }
            Expression::Prefix {
                operator,
                expression,
                location,
            } => {
                let (r#type, expression) = self.resolve_expression(*expression);
                let expected_type = match operator {
                    "!" => dummy_int_signed,
                    "+" | "-" | "++" | "--" => dummy_double,
                    _ => panic!("impossible"),
                };
                if let Err(_) = self.unify_types(expected_type, r#type) {
                    let message = format!(
                        "Expect `{}` after `{}`, get `{}`.",
                        expected_type, operator, r#type
                    );
                    self.push_error(&message, location);
                }
                (
                    r#type,
                    Expression::Prefix {
                        operator,
                        expression: Box::new(expression),
                        location,
                    },
                )
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let (type_left, expr_left) = self.resolve_expression(*left);
                let (type_right, expr_right) = self.resolve_expression(*right);
                let r#type = match operator {
                    "=" => match self.unify_types(type_left, type_right) {
                        Ok((type_left, _)) => type_left,
                        Err(_) => {
                            let message =
                                format!("`{}` and `{}` cannot be unified.", type_left, type_right);
                            self.push_error(&message, location);
                            dummy_t
                        }
                    },
                    "+=" | "-=" | "*=" | "/=" | "%=" | "+" | "-" | "*" | "/" | "%" => {
                        if let Err(_) = self.unify_types(dummy_double, type_right) {
                            let message = format!("Expect a number after `{}`.", operator);
                            self.push_error(&message, location);
                            dummy_t
                        } else {
                            match self.unify_types(type_left, type_right) {
                                Ok((type_left, _)) => type_left,
                                Err(_) => {
                                    let message = format!(
                                        "`{}` and `{}` cannot be unified.",
                                        type_left, type_right
                                    );
                                    self.push_error(&message, location);
                                    dummy_t
                                }
                            }
                        }
                    }
                    "||" | "&&" => {
                        if let Err(_) = self.unify_types(dummy_int_signed, type_right) {
                            let message =
                                format!("Expect {} after `{}`.", dummy_int_signed, operator);
                            self.push_error(&message, location);
                            dummy_int_signed
                        } else {
                            match self.unify_types(dummy_int_signed, type_left) {
                                Ok(_) => dummy_int_signed,
                                Err(_) => {
                                    let message = format!(
                                        "Expect {} before `{}`.",
                                        dummy_int_signed, operator
                                    );
                                    self.push_error(&message, location);
                                    dummy_t
                                }
                            }
                        }
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        if let Err(_) = self.unify_types(type_left, type_right) {
                            let message =
                                format!("`{}` and `{}` cannot be unified.", type_left, type_right);
                            self.push_error(&message, location);
                        }
                        dummy_int_signed
                    }
                    _ => panic!("impossible"),
                };
                (
                    r#type,
                    Expression::Infix {
                        left: Box::new(expr_left),
                        operator,
                        right: Box::new(expr_right),
                    },
                )
            }
            Expression::Suffix {
                operator,
                expression,
            } => {
                let (r#type, expression) = self.resolve_expression(*expression);
                let r#type = match self.unify_types(dummy_double, r#type) {
                    Ok(_) => r#type,
                    Err(_) => {
                        let message = format!("Expect a number before `{}`.", operator);
                        self.push_error(&message, location);
                        dummy_t
                    }
                };
                (
                    r#type,
                    Expression::Suffix {
                        operator,
                        expression: Box::new(expression),
                    },
                )
            }
            Expression::Index { expression, index } => {
                let (type_expr, expression) = self.resolve_expression(*expression);
                let (type_index, index) = self.resolve_expression(*index);
                let r#type = if type_expr.get_array().0 {
                    match type_index {
                        Type::Char { .. }
                        | Type::Short { .. }
                        | Type::Int { .. }
                        | Type::Long { .. } => {}
                        _ => {
                            let message = format!(
                                "Expect an integer as the array index, get `{}`.",
                                type_index
                            );
                            self.push_error(&message, location);
                        }
                    }
                    type_expr
                } else {
                    let message = format!("Expect an array, get `{}`.", type_expr);
                    self.push_error(&message, location);
                    dummy_t
                };
                (
                    r#type,
                    Expression::Index {
                        expression: Box::new(expression),
                        index: Box::new(index),
                    },
                )
            }
            Expression::Call {
                expression,
                arguments,
            } => {
                let name = match *expression {
                    Expression::Ident { value, .. } => value,
                    _ => panic!("The function name in a function call now must be a StrConst."),
                };
                let (return_type, parameters) = match self.symbol_table.functions.get(name) {
                    None => {
                        let message = format!("Undefined function `{:?}`.", name);
                        self.push_error(&message, location);
                        return (
                            dummy_t,
                            Expression::Call {
                                expression: Box::new(Expression::Ident {
                                    value: name,
                                    location,
                                }),
                                arguments,
                            },
                        );
                    }
                    Some((return_type, parameters)) => (*return_type, parameters.clone()),
                };
                let arguments: Vec<_> = arguments
                    .into_iter()
                    .zip(parameters.iter())
                    .map(|(arg, (param, type_param))| {
                        let (type_arg, arg) = self.resolve_expression(*arg);
                        if let Err(_) = self.unify_types(*type_param, type_arg) {
                            let message = format!(
                                "Expect `{}` for `{}`, get `{}`.",
                                type_param, param, type_arg
                            );
                            self.push_error(&message, location);
                        }
                        Box::new(arg)
                    })
                    .collect();
                (
                    return_type,
                    Expression::Call {
                        expression: Box::new(Expression::Ident {
                            value: name,
                            location,
                        }),
                        arguments,
                    },
                )
            }
            Expression::InitList { expressions, .. } => {
                let mut err_flag = false;
                let mut r#type = dummy_t;
                let expressions: Vec<_> = expressions
                    .into_iter()
                    .map(|expr| {
                        let (typ, expr) = self.resolve_expression(*expr);
                        match self.unify_types(r#type, typ) {
                            Ok((typ, _)) => {
                                r#type = typ;
                            }
                            Err(_) => {
                                err_flag = true;
                                let message = format!(
                                    "Inconsistent types in the init list: `{}` and `{}`.",
                                    r#type, typ
                                );
                                self.push_error(&message, location);
                            }
                        }
                        Box::new(expr)
                    })
                    .collect();
                if err_flag {
                    (
                        dummy_t,
                        Expression::InitList {
                            expressions,
                            location,
                        },
                    )
                } else {
                    (
                        r#type,
                        Expression::InitList {
                            expressions,
                            location,
                        },
                    )
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn multiple_functions() {
        let source = "void f() {}\nvoid f() {}";
        let expected_errors = vec![Error::Resolving {
            message: "Multiple function definitions.".to_string(),
            location: Location::new(2, 1),
        }];
        let expected_ast = vec![
            Function {
                r#type: Type::Void {
                    array_flag: false,
                    array_len: None,
                    location: Location::new(1, 1),
                },
                name: "f",
                parameters: IndexMap::new(),
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(1, 10),
                },
                location: Location::new(1, 1),
            },
            Function {
                r#type: Type::Void {
                    array_flag: false,
                    array_len: None,
                    location: Location::new(2, 1),
                },
                name: "f",
                parameters: IndexMap::new(),
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(2, 10),
                },
                location: Location::new(2, 1),
            },
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, errors) = Resolver::new(generic_ast, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn unresolved() {
        let source = "T f(T a) {}";
        let expected_errors = vec![Error::Resolving {
            message: "Unresolved return type of function `f`.".to_string(),
            location: Location::new(1, 1),
        }, Error::Resolving {
            message: "Unresolved parameter `a` in function `f`.".to_string(),
            location: Location::new(1, 1),
        }];
        let expected_ast = vec![
            Function {
                r#type: Type::T {
                    dummy_flag: false,
                    array_flag: false,
                    array_len: None,
                    location: Location::new(1, 1),
                },
                name: "f",
                parameters: [(
                    "a",
                    Type::T {
                        dummy_flag: false,
                        array_flag: false,
                        array_len: None,
                        location: Location::new(1, 5),
                    },
                )]
                .iter()
                .cloned()
                .collect(),
                body: Statement::Block {
                    statements: vec![],
                    location: Location::new(1, 10),
                },
                location: Location::new(1, 1),
            }
        ];
        let errors = Vec::new();
        let (tokens, errors) = Lexer::new(&source, errors).run();
        let (generic_ast, errors) = Parser::new(tokens, errors).run();
        let (ast, errors) = Resolver::new(generic_ast, errors).run();
        assert_eq!(errors, expected_errors);
        assert_eq!(ast, expected_ast);
    }
}
