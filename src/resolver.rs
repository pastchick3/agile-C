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
        self.symbol_table.enter();
        for (param, type_) in parameters.iter() {
            self.symbol_table.insert(param, *type_);
        }
        let body = self.resolve_statement(body);
        let mut new_parameters = IndexMap::new();
        for param in parameters.keys() {
            let type_ = self.symbol_table.get(param).unwrap();
            new_parameters.insert(*param, type_);
        }
        self.symbol_table.update_parameters(new_parameters);
        self.symbol_table.leave();
        Function { type_, name, parameters, body, location }
    }

    fn resolve_statement(&mut self, stmt: Statement<'a>) -> Statement<'a> {
        match stmt {
            st @ Statement::Continue(_) => st,
            st @ Statement::Break(_) => st,
            Statement::Expr(expr) => {
                let (_, expr) = self.resolve_expression(expr);
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
                    Ok((return_type, _)) => self.symbol_table.update_return_type(return_type),
                    Err(_) => {
                        let message = format!("`{:?}` is expected to return `{:?}`, get `{:?}`.",
                                        self.symbol_table.current_func, return_type, type_);
                        self.push_error(&message, location);
                    },
                }
                Statement::Return { expr, location }
            },
            Statement::Block { statements, location } => {
                let mut sts = Vec::new();
                self.symbol_table.enter();
                for st in statements.into_iter() {
                    sts.push(Box::new(self.resolve_statement(*st)));
                }
                self.symbol_table.leave();
                Statement::Block { statements: sts, location }
            },
            Statement::Def { type_, declarators, location } => {
                let mut decls = Vec::new();
                for (ident, init) in declarators.into_iter() {
                    let (typ, expr) = match init {
                        Some(expr) => {
                            let (t, e) = self.resolve_expression(expr);
                            (t, Some(e))
                        },
                        None => (Type::T(Location::empty()), None),
                    };
                    match self.unify_types(type_, typ) {
                        Ok(_) => {},
                        Err(_) => {
                            let message = format!("Expect `{:?}` to be `{:?}`, get `{:?}`.", ident, type_, typ);
                            self.push_error(&message, location);
                        },
                    }
                    self.symbol_table.insert(ident, type_);
                    decls.push((ident, expr));
                }
                Statement::Def { type_, declarators: decls, location }
            },
            Statement::While { condition, body, location } => {
                let (type_, condition) = self.resolve_expression(condition);
                match self.unify_types(Type::Int(Location::empty()), type_) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect loop condition to be `Int`, get `{:?}`.", type_);
                        self.push_error(&message, location);
                    },
                }
                let body = Box::new(self.resolve_statement(*body));
                Statement::While { condition, body, location }
            },
            Statement::Do { condition, body, location } => {
                let (type_, condition) = self.resolve_expression(condition);
                match self.unify_types(Type::Int(Location::empty()), type_) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect loop condition to be `Int`, get `{:?}`.", type_);
                        self.push_error(&message, location);
                    },
                }
                let body = Box::new(self.resolve_statement(*body));
                Statement::Do { condition, body, location }
            },
            Statement::For { initialization, condition, increment, body, location } => {
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
                Statement::For { initialization, condition, increment, body, location }
            },
            Statement::If { condition, body, alternative, location } => {
                let (type_, condition) = self.resolve_expression(condition);
                match self.unify_types(Type::Int(Location::empty()), type_) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect loop condition to be `Int`, get `{:?}`.", type_);
                        self.push_error(&message, location);
                    },
                }
                let body = Box::new(self.resolve_statement(*body));
                let alternative = match alternative {
                    Some(stmt) => Some(Box::new(self.resolve_statement(*stmt))),
                    None => None,
                };
                Statement::If { condition, body, alternative, location }
            },
            Statement::Switch { expression, branches, default, location } => {
                let (_, expression) = self.resolve_expression(expression);
                let mut branches_ = Vec::new();
                for (label, stmts) in branches.into_iter() {
                    let label_ = self.resolve_expression(label).1;
                    let mut stmts_ = Vec::new();
                    for stmt in stmts.into_iter() {
                        stmts_.push(Box::new(self.resolve_statement(*stmt)));
                    };
                    branches_.push((label_, stmts_));
                }
                let default = match default {
                    Some(stmts) => {
                        let mut stmts_ = Vec::new();
                        for stmt in stmts.into_iter() {
                            stmts_.push(Box::new(self.resolve_statement(*stmt)));
                        }
                        Some(stmts_)
                    },
                    None => None,
                };
                Statement::Switch { expression, branches: branches_, default, location }
            },
        }
    }

    fn resolve_expression(&mut self, expr: Expression<'a>) -> (Type, Expression<'a>) {
        match expr {
            Expression::Ident { value, location } => {
                match self.symbol_table.get(value) {
                    Some(type_) => (type_, Expression::Ident { value, location }),
                    None => {
                        let message = format!("Undefined ident `{:?}`.", value);
                        self.push_error(&message, location);
                        (Type::T(Location::empty()), Expression::Ident { value, location })
                    },
                }
            },
            ex @ Expression::IntConst { .. } => (Type::Int(Location::empty()), ex),
            ex @ Expression::FloatingConst { .. } => (Type::Float(Location::empty()), ex),
            ex @ Expression::CharConst { .. } => (Type::Char(Location::empty()), ex),
            Expression::StrConst { .. } => panic!("Resolver for StrConst is not implemented."),
            Expression::Prefix { operator, expression, location } => {
                let (type_, expression) = self.resolve_expression(*expression);
                match self.unify_types(Type::Double(Location::empty()), type_) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect numbers after `prefix {:?}`, get `{:?}`.", operator, type_);
                        self.push_error(&message, location);
                    },
                }
                (type_, Expression::Prefix { operator, expression: Box::new(expression), location })
            },
            Expression::Infix { left, operator, right } => {
                let location = left.locate();
                let (type_l, expr_l) = self.resolve_expression(*left);
                let (type_r, expr_r) = self.resolve_expression(*right);
                match self.unify_types(Type::Double(Location::empty()), type_l) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect numbers before `infix {:?}`, get `{:?}`.", operator, type_l);
                        self.push_error(&message, location);
                    },
                }
                match self.unify_types(Type::Double(Location::empty()), type_r) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect numbers after `infix {:?}`, get `{:?}`.", operator, type_r);
                        self.push_error(&message, location);
                    },
                }
                (type_r, Expression::Infix { left: Box::new(expr_l), operator, right: Box::new(expr_r) })
            },
            Expression::Suffix { operator, expression } => {
                let location = expression.locate();
                let (type_, expression) = self.resolve_expression(*expression);
                match self.unify_types(Type::Double(Location::empty()), type_) {
                    Ok(_) => {},
                    Err(_) => {
                        let message = format!("Expect numbers before `suffix {:?}`, get `{:?}`.", operator, type_);
                        self.push_error(&message, location);
                    },
                }
                (type_, Expression::Suffix { operator, expression: Box::new(expression) })
            },
            Expression::Index { .. } => {
                panic!("Expression::Index not implemented!");
            },
            Expression::Call { name, arguments } => {
                let (name, location) = match *name {
                    Expression::Ident{ value, location } => (value, location),
                    _ => panic!("Call only with str const"),
                };
                let (return_type, parameters): (Type, Vec<Type>) = match self.symbol_table.functions.get(name) {
                    None => {
                        let message = format!("Undefined function `{:?}`.", name);
                        self.push_error(&message, location);
                        return (Type::T(Location::empty()), Expression::Call { name: Box::new(Expression::Ident{ value: name, location }), arguments });
                    },
                    Some((return_type, parameters)) => {
                        let mut parameters_ = Vec::new();
                        for param in parameters.values() {
                            parameters_.push(*param);
                        }
                        (*return_type, parameters_)
                    },
                };
                let mut arguments_ = Vec::new();
                for (arg, type_param) in arguments.into_iter().zip(parameters.iter()) {
                    let (type_arg, arg) = self.resolve_expression(*arg);
                    match self.unify_types(*type_param, type_arg) {
                        Ok(_) => {},
                        Err(_) => {
                            let message = format!("Unmatched arguments in `{:?}`.", name);
                            self.push_error(&message, location);
                        },
                    }
                    arguments_.push(Box::new(arg));
                }
                (return_type, Expression::Call { name: Box::new(Expression::Ident{ value: name, location }), arguments: arguments_ })
            },
        }
    }
}
