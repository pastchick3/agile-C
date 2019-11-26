use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::structure::{
    Array, Error, Expression, Function, Locate, Location, Statement, StaticObject, Type,
    TypeRelationship,
};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum Symbol {
    Variable {
        scope: Option<usize>,
        function: String,
        name: String,
    },
    Expression {
        scope: Option<usize>,
        count: usize,
    },
    Parameter {
        scope: Option<usize>,
        function: String,
        name: String,
    },
    Return {
        scope: Option<usize>,
        function: String,
    },
}

impl Symbol {
    fn get_scope(&self) -> Option<usize> {
        match self {
            Symbol::Variable { scope, .. } => scope.clone(),
            Symbol::Expression { scope, .. } => scope.clone(),
            Symbol::Parameter { scope, .. } => scope.clone(),
            Symbol::Return { scope, .. } => scope.clone(),
        }
    }
}

#[derive(Clone, Debug)]
enum Bound {
    Type(Type),
    Symbol(Symbol),
}

#[derive(Clone, Debug)]
struct TypeBound {
    upper: Vec<Bound>,
    lower: Vec<Bound>,
    bounded: Option<Type>,
    wrapped: Option<Rc<RefCell<Type>>>,
}

impl TypeBound {
    fn new(wrapped: Rc<RefCell<Type>>) -> TypeBound {
        let type_ = Type::clone(&wrapped.borrow());
        let bounded = if let Type::T { .. } = type_ {
            None
        } else {
            Some(type_)
        };
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            bounded,
            wrapped: Some(wrapped),
        }
    }

    fn bounded(bounded: Type) -> TypeBound {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            bounded: Some(bounded),
            wrapped: None,
        }
    }

    fn squeezed(bound: Bound) -> TypeBound {
        TypeBound {
            upper: vec![bound.clone()],
            lower: vec![bound.clone()],
            bounded: None,
            wrapped: None,
        }
    }

    fn merge(left: &TypeBound, right: &TypeBound, wrapped_flag: &str) -> TypeBound {
        let bounded = if let (
            TypeBound {
                bounded: Some(left),
                ..
            },
            TypeBound {
                bounded: Some(right),
                ..
            },
        ) = (left, right)
        {
            match Type::compare_types(left, right) {
                TypeRelationship::Sub => Some(right.clone()),
                TypeRelationship::Base => Some(left.clone()),
                TypeRelationship::Equal => Some(right.clone()),
                TypeRelationship::None => None,
            }
        } else if let TypeBound {
            bounded: Some(type_),
            ..
        } = left
        {
            Some(type_.clone())
        } else if let TypeBound {
            bounded: Some(type_),
            ..
        } = right
        {
            Some(type_.clone())
        } else {
            None
        };
        let mut upper = left.upper.to_vec();
        upper.append(&mut right.upper.to_vec());
        let mut lower = left.lower.to_vec();
        lower.append(&mut right.lower.to_vec());
        let wrapped = match wrapped_flag {
            "left" => left.wrapped.clone(),
            "right" => right.wrapped.clone(),
            _ => None,
        };
        TypeBound {
            upper,
            lower,
            bounded,
            wrapped,
        }
    }
}

#[derive(Debug)]
struct Scope {
    index: usize,
    outer: Option<usize>,
    symbols: HashMap<Symbol, TypeBound>,
    expr_counter: usize,
    structures: Vec<Type>,
}

impl Scope {
    fn new(index: usize, outer: Option<usize>) -> Scope {
        Scope {
            index,
            outer,
            symbols: HashMap::new(),
            expr_counter: 0,
            structures: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct SymbolTable {
    current_func: Option<String>,
    current_scope: Option<usize>,
    func_returns: HashMap<String, TypeBound>,
    func_params: HashMap<String, IndexMap<String, TypeBound>>,
    scopes: Vec<Scope>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            current_func: None,
            current_scope: None,
            func_returns: HashMap::new(),
            func_params: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    fn get_current_scope(&self) -> Option<&Scope> {
        let current = self.current_scope?;
        self.scopes.get(current)
    }

    fn get_scope(&self, index: Option<usize>) -> Option<&Scope> {
        self.scopes.get(index?)
    }

    fn get_current_scope_mut(&mut self) -> Option<&mut Scope> {
        let current = self.current_scope?;
        self.scopes.get_mut(current)
    }

    fn get_scope_mut(&mut self, index: Option<usize>) -> Option<&mut Scope> {
        self.scopes.get_mut(index?)
    }

    fn enter_scope(&mut self) {
        let outer = self.current_scope.clone();
        let current = self.scopes.len();
        self.current_scope = Some(current);
        self.scopes.push(Scope::new(current, outer));
    }

    fn leave_scope(&mut self) {
        self.current_scope = self
            .get_current_scope()
            .expect("Try to leave the global scope.")
            .outer;
    }

    fn is_parameter(&self, name: &str) -> bool {
        let current_func = self.current_func.as_ref().unwrap();
        self.func_params
            .get(current_func)
            .unwrap()
            .contains_key(name)
    }

    fn make_expression_symbol(&mut self) -> Symbol {
        let current_scope = self.current_scope.expect("No scope");
        let scope = self.get_current_scope_mut().expect("No scope");
        let symbol = Symbol::Expression {
            scope: Some(current_scope),
            count: scope.expr_counter,
        };
        scope.expr_counter += 1;
        symbol
    }

    fn define_function(&mut self, function: &Function) {
        let Function {
            return_type,
            name,
            parameters,
            ..
        } = function;
        self.func_returns
            .insert(name.clone(), TypeBound::new(Rc::clone(return_type)));
        let parameters: IndexMap<_, _> = parameters
            .iter()
            .map(|(param, type_)| (param.clone(), TypeBound::new(Rc::clone(type_))))
            .collect();
        self.func_params.insert(name.clone(), parameters);
    }

    fn define_symbol(&mut self, symbol: Symbol, type_bound: TypeBound) {
        let scope = self.get_current_scope_mut().expect("No scope");
        scope.symbols.insert(symbol, type_bound);
    }

    fn define_structure(&mut self, structure: &Type) {
        let scope = self.get_current_scope_mut().expect("No scope");
        if let Type::Struct { .. } = structure {
            scope.structures.push(structure.clone());
        }
    }

    fn get_type_bound(&mut self, symbol: &Symbol) -> Option<TypeBound> {
        let scope = self.get_scope(symbol.get_scope()).expect("None scope");
        scope.symbols.get(symbol).cloned()
    }

    fn update_type_bound(&mut self, symbol: Symbol, type_bound: TypeBound) {
        let index = symbol.get_scope();
        let scope = self.get_scope_mut(index).expect("No scope.");
        scope.symbols.insert(symbol, type_bound);
    }

    fn get_all_symbols(&self) -> Vec<(Symbol, TypeBound)> {
        self.scopes
            .iter()
            .map(|scope| scope.symbols.clone().into_iter())
            .flatten()
            .collect::<Vec<_>>()
    }

    fn update_global_symbols(&mut self) {
        let symbols = self.get_all_symbols();
        for (symbol, type_bound) in symbols.iter() {
            match symbol {
                Symbol::Parameter { function, name, .. } => {
                    let old_type_bound = self.func_params.get(function).unwrap().get(name).unwrap();
                    let type_bound = TypeBound::merge(old_type_bound, type_bound, "left");
                    self.func_params
                        .get_mut(function)
                        .unwrap()
                        .insert(name.to_string(), type_bound.clone());
                }
                Symbol::Return { function, .. } => {
                    let old_type_bound = self.func_returns.get(function).unwrap();
                    let type_bound = TypeBound::merge(old_type_bound, type_bound, "left");
                    self.func_returns
                        .insert(function.to_string(), type_bound.clone());
                }
                _ => (),
            }
        }
        for (symbol, _) in symbols.into_iter() {
            match &symbol {
                Symbol::Parameter { function, name, .. } => {
                    let type_bound = self
                        .func_params
                        .get(function)
                        .unwrap()
                        .get(name)
                        .unwrap()
                        .clone();
                    self.update_type_bound(symbol, type_bound);
                }
                Symbol::Return { function, .. } => {
                    let type_bound = self.func_returns.get(function).unwrap().clone();
                    self.update_type_bound(symbol, type_bound);
                }
                _ => (),
            }
        }
    }

    fn update_wrapped(&mut self) {
        for (_, type_bound) in self.get_all_symbols() {
            let TypeBound {
                wrapped, bounded, ..
            } = type_bound;
            if wrapped.is_some() && bounded.is_some() {
                let mut old_type = wrapped.as_ref().unwrap().borrow_mut();
                if !old_type.specialized() {
                    old_type.set_specialized(bounded.unwrap());
                }
            }
        }
    }
}

pub struct Resolver<'a> {
    symbol_table: SymbolTable,
    generic_ast: Option<Vec<StaticObject>>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<StaticObject>, errors: &'a mut Vec<Error>) -> Resolver<'a> {
        Resolver {
            symbol_table: SymbolTable::new(),
            generic_ast: Some(generic_ast),
            errors,
        }
    }

    pub fn run(&mut self) -> Result<Vec<StaticObject>, ()> {
        self.symbol_table.enter_scope();
        let ast: Vec<_> = self.generic_ast.take().unwrap();
        for obj in &ast {
            match obj {
                StaticObject::Type(structure) => self.symbol_table.define_structure(structure),
                StaticObject::Function(function) => {
                    self.symbol_table.define_function(function);
                    self.resolve_function(function);
                }
            }
        }
        self.symbol_table.leave_scope();
        self.resolve_symbols();
        self.symbol_table.update_wrapped();
        if self.errors.is_empty() {
            Ok(ast)
        } else {
            Err(())
        }
    }

    fn push_error(&mut self, message: &str, location: &Location) {
        self.errors.push(Error::Resolving {
            message: message.to_string(),
            location: location.clone(),
        });
    }

    fn resolve_symbols(&mut self) {
        let mut modified = true;
        while modified {
            modified = false;
            self.symbol_table.update_global_symbols();
            let mut symbols = self.symbol_table.get_all_symbols();
            for (symbol, type_bound) in symbols.iter_mut() {
                self.resolve_type_bound(symbol, type_bound, &mut modified, false);
            }
        }
        modified = true;
        while modified {
            modified = false;
            self.symbol_table.update_global_symbols();
            let mut symbols = self.symbol_table.get_all_symbols();
            for (symbol, type_bound) in symbols.iter_mut() {
                self.resolve_type_bound(symbol, type_bound, &mut modified, true);
            }
        }
    }

    fn resolve_type_bound(
        &mut self,
        symbol: &Symbol,
        type_bound: &mut TypeBound,
        modified: &mut bool,
        aggressive: bool,
    ) {
        let mut upper_type = Type::Any;
        for bound in type_bound.upper.iter() {
            let type_ = match bound {
                Bound::Type(type_) => Some(type_).cloned(),
                Bound::Symbol(symbol) => self
                    .symbol_table
                    .get_type_bound(symbol)
                    .unwrap()
                    .bounded
                    .clone(),
            };
            match type_ {
                None => {
                    if !aggressive {
                        return;
                    }
                }
                Some(type_) => match Type::compare_types(&mut upper_type, &type_) {
                    TypeRelationship::Sub => (),
                    TypeRelationship::Base => {
                        upper_type = type_;
                    }
                    TypeRelationship::Equal => (),
                    TypeRelationship::None => {
                        let message =
                            format!("Incompatible types `{:?}` and `{:?}`.", upper_type, type_);
                        self.push_error(&message, &type_.locate());
                        return;
                    }
                },
            }
        }
        let mut lower_type = Type::Nothing;
        for bound in type_bound.lower.iter() {
            let type_ = match bound {
                Bound::Type(type_) => Some(type_).cloned(),
                Bound::Symbol(symbol) => self
                    .symbol_table
                    .get_type_bound(symbol)
                    .unwrap()
                    .bounded
                    .clone(),
            };
            match type_ {
                None => {
                    if !aggressive {
                        return;
                    }
                }
                Some(type_) => match Type::compare_types(&mut lower_type, &type_) {
                    TypeRelationship::Sub => {
                        lower_type = type_;
                    }
                    TypeRelationship::Base => (),
                    TypeRelationship::Equal => (),
                    TypeRelationship::None => {
                        let message =
                            format!("Incompatible types `{:?}` and `{:?}`.", lower_type, type_);
                        self.push_error(&message, &type_.locate());
                        return;
                    }
                },
            }
        }
        let (lower_type, upper_type) =
            match Type::instantiate_any_nothing(lower_type.clone(), upper_type.clone()) {
                Ok((lower_type, upper_type)) => (lower_type, upper_type),
                Err(_) => {
                    return;
                }
            };
        if let Some(old_type) = &type_bound.bounded {
            match Type::compare_types(old_type, &upper_type) {
                TypeRelationship::Sub | TypeRelationship::Equal => {
                    match Type::compare_types(old_type, &lower_type) {
                        TypeRelationship::Base | TypeRelationship::Equal => {
                            return;
                        }
                        _ => {
                            let message = format!(
                                "Incompatible types `{:?}` and `{:?}`.",
                                old_type, lower_type
                            );
                            self.push_error(&message, &old_type.locate());
                            return;
                        }
                    }
                }
                _ => {
                    let message = format!(
                        "Incompatible types `{:?}` and `{:?}`.",
                        old_type, upper_type
                    );
                    self.push_error(&message, &old_type.locate());
                    return;
                }
            }
        }
        let type_ = match Type::compare_types(&lower_type, &upper_type) {
            TypeRelationship::Sub | TypeRelationship::Equal => lower_type,
            _ => {
                let message = format!(
                    "Incompatible types `{:?}` and `{:?}`.",
                    lower_type, upper_type
                );
                self.push_error(&message, &lower_type.locate());
                return;
            }
        };
        type_bound.bounded = Some(type_.clone());
        self.symbol_table
            .update_type_bound(symbol.clone(), type_bound.clone());
        *modified = true;
    }

    fn resolve_function(
        &mut self,
        Function {
            return_type,
            name,
            parameters,
            body,
            ..
        }: &Function,
    ) {
        self.symbol_table.current_func = Some(name.clone());
        self.symbol_table.enter_scope();
        for (name, type_) in parameters {
            let symbol = Symbol::Parameter {
                function: self.symbol_table.current_func.clone().unwrap(),
                scope: self.symbol_table.current_scope,
                name: name.clone(),
            };
            let type_bound = TypeBound::new(Rc::clone(type_));
            self.symbol_table.define_symbol(symbol, type_bound);
        }
        let symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: name.clone(),
        };
        let type_bound = TypeBound::new(Rc::clone(return_type));
        self.symbol_table.define_symbol(symbol, type_bound);
        self.resolve_statement(body);
        self.symbol_table.leave_scope();
        self.symbol_table.current_func = None;
    }

    fn resolve_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Null(_) => (),
            Statement::Continue(_) => (),
            Statement::Break(_) => (),
            Statement::Expr(expr) => match self.resolve_expression(expr) {
                _ => (),
            },
            Statement::Return { expression, .. } => self.resolve_statement_return(expression),
            Statement::Block { statements, .. } => self.resolve_statement_block(statements),
            Statement::Def {
                base_type,
                declarators,
                ..
            } => self.resolve_statement_def(base_type, declarators),
            Statement::While {
                condition, body, ..
            } => self.resolve_statement_while(condition, body),
            Statement::Do {
                condition, body, ..
            } => self.resolve_statement_while(condition, body),
            Statement::For {
                initialization,
                condition,
                increment,
                body,
                ..
            } => self.resolve_statement_for(initialization, condition, increment, body),
            Statement::If {
                condition,
                body,
                alternative,
                ..
            } => self.resolve_statement_if(condition, body, alternative),
            Statement::Switch {
                expression,
                branches,
                default,
                ..
            } => self.resolve_statement_switch(expression, branches, default),
        }
    }

    fn resolve_statement_return(&mut self, expression: &Option<Expression>) {
        let current_func = self.symbol_table.current_func.as_ref().unwrap().to_string();
        let return_symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: current_func.clone(),
        };
        let mut return_type_bound = self.symbol_table.get_type_bound(&return_symbol).unwrap();
        match expression {
            Some(expr) => {
                if let Ok((expr_symbol, mut expr_type_bound)) = self.resolve_expression(expr) {
                    expr_type_bound
                        .upper
                        .push(Bound::Symbol(return_symbol.clone()));
                    return_type_bound
                        .lower
                        .push(Bound::Symbol(expr_symbol.clone()));
                    self.symbol_table
                        .update_type_bound(expr_symbol, expr_type_bound);
                }
            }
            None => {
                let type_void = Type::Void {
                    array_flag: false,
                    array_len: None,
                    pointer_flag: false,
                    location: Location::default(),
                };
                return_type_bound.upper.push(Bound::Type(type_void.clone()));
                return_type_bound.lower.push(Bound::Type(type_void.clone()));
            }
        }
        self.symbol_table
            .update_type_bound(return_symbol, return_type_bound);
    }

    fn resolve_statement_block(&mut self, statements: &Vec<Statement>) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement_def(
        &mut self,
        base_type: &Rc<RefCell<Type>>,
        declarators: &Vec<(Rc<RefCell<Type>>, String, Option<Expression>)>,
    ) {
        let mut struct_flag = false;
        let struct_members = if let Type::Struct { members, .. } = base_type.borrow().clone() {
            self.symbol_table
                .define_structure(&base_type.borrow().clone());
            struct_flag = true;
            Some(members.clone())
        } else {
            None
        };
        for (type_, name, init) in declarators {
            let array_flag = type_.borrow().get_array().0;
            let symbol = Symbol::Variable {
                function: self.symbol_table.current_func.clone().unwrap(),
                scope: self.symbol_table.current_scope,
                name: name.clone(),
            };
            let mut type_bound = TypeBound::new(Rc::clone(type_));
            if let Some(expr) = init {
                if let Expression::InitList { pairs, .. } = expr {
                    if !struct_flag && !array_flag {
                        let message = "Unexpect InitList.";
                        self.push_error(&message, &type_.borrow().locate());
                        return;
                    }
                    let pairs = self.resolve_expression_initlist(pairs);
                    if struct_flag {
                        for (name, expr) in pairs {
                            if name.is_none() {
                                let message = "Unnamed field in InitList.";
                                self.push_error(&message, &type_.borrow().locate());
                                return;
                            }
                            let name = name.unwrap();
                            if expr.is_err() {
                                return;
                            }
                            let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                            match struct_members.as_ref().unwrap().get(&name) {
                                Some(type_) => {
                                    expr_type_bound.upper.push(Bound::Type(type_.clone()))
                                }
                                None => {
                                    let message = "Unknown field in InitList.";
                                    self.push_error(&message, &type_.borrow().locate());
                                    return;
                                }
                            }
                            self.symbol_table
                                .update_type_bound(expr_symbol, expr_type_bound);
                        }
                    } else {
                        for (name, expr) in pairs {
                            if name.is_some() {
                                let message = "No field in InitList of array.";
                                self.push_error(&message, &type_.borrow().locate());
                                return;
                            }
                            if expr.is_err() {
                                return;
                            }
                            let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                            expr_type_bound
                                .upper
                                .push(Bound::Type(base_type.borrow().clone()));
                            self.symbol_table
                                .update_type_bound(expr_symbol, expr_type_bound);
                        }
                    }
                } else {
                    if struct_flag || array_flag {
                        let message = "Expect InitList.";
                        self.push_error(&message, &type_.borrow().locate());
                        return;
                    }
                    if let Ok((expr_symbol, mut expr_type_bound)) = self.resolve_expression(expr) {
                        let bound = Bound::Symbol(expr_symbol.clone());
                        type_bound.lower.push(bound);
                        expr_type_bound.upper.push(Bound::Symbol(symbol.clone()));
                        self.symbol_table
                            .update_type_bound(expr_symbol, expr_type_bound);
                    }
                }
            };
            self.symbol_table.define_symbol(symbol, type_bound);
        }
    }

    fn resolve_statement_while(&mut self, condition: &Expression, body: &Statement) {
        match self.resolve_expression(condition) {
            _ => (),
        };
        self.resolve_statement(body);
    }

    fn resolve_statement_for(
        &mut self,
        initialization: &Option<Box<Statement>>,
        condition: &Option<Expression>,
        increment: &Option<Expression>,
        body: &Statement,
    ) {
        if let Some(stmt) = initialization {
            self.resolve_statement(stmt);
        }
        if let Some(expr) = condition {
            match self.resolve_expression(expr) {
                _ => (),
            }
        }
        if let Some(expr) = increment {
            match self.resolve_expression(expr) {
                _ => (),
            }
        }
        self.resolve_statement(body);
    }

    fn resolve_statement_if(
        &mut self,
        condition: &Expression,
        body: &Statement,
        alternative: &Option<Box<Statement>>,
    ) {
        match self.resolve_expression(condition) {
            _ => (),
        };
        self.resolve_statement(body);
        if let Some(stmt) = alternative {
            self.resolve_statement(stmt);
        }
    }

    fn resolve_statement_switch(
        &mut self,
        expression: &Expression,
        branches: &Vec<(Expression, Vec<Statement>)>,
        default: &Option<Vec<Statement>>,
    ) {
        match self.resolve_expression(expression) {
            _ => (),
        };
        for (expr, stmts) in branches {
            match self.resolve_expression(expr) {
                _ => (),
            };
            for stmt in stmts {
                self.resolve_statement(stmt);
            }
        }
        if let Some(stmts) = default {
            for stmt in stmts {
                self.resolve_statement(stmt);
            }
        }
    }

    fn resolve_expression(&mut self, expression: &Expression) -> Result<(Symbol, TypeBound), ()> {
        match expression {
            Expression::Ident { value, location } => self.resolve_expression_ident(value, location),
            Expression::IntConst { value, location } => {
                self.resolve_expression_intconst(value, location)
            }
            Expression::FloatConst { value, location } => {
                self.resolve_expression_floatconst(value, location)
            }
            Expression::CharConst { .. } => self.resolve_expression_charconst(),
            Expression::StrConst { .. } => self.resolve_expression_strconst(),
            Expression::Prefix {
                operator,
                expression,
                ..
            } => self.resolve_expression_prefix(operator, expression),
            Expression::Infix {
                left,
                operator,
                right,
            } => self.resolve_expression_infix(left, operator, right),
            Expression::Suffix {
                operator,
                expression,
            } => self.resolve_expression_suffix(operator, expression),
            Expression::Group { expression, .. } => self.resolve_expression_group(expression),
            Expression::Index { expression, index } => {
                self.resolve_expression_index(expression, index)
            }
            Expression::Call {
                expression,
                arguments,
            } => self.resolve_expression_call(expression, arguments),
            Expression::InitList { .. } => panic!("Direct call to InitList."),
        }
    }

    fn resolve_expression_ident(
        &mut self,
        value: &str,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let symbol = if self.symbol_table.is_parameter(value) {
            Symbol::Parameter {
                function: self.symbol_table.current_func.clone().unwrap(),
                scope: self.symbol_table.current_scope,
                name: value.to_string(),
            }
        } else {
            Symbol::Variable {
                function: self.symbol_table.current_func.clone().unwrap(),
                scope: self.symbol_table.current_scope,
                name: value.to_string(),
            }
        };
        self.symbol_table
            .get_type_bound(&symbol)
            .map(|type_bound| (symbol, type_bound))
            .ok_or_else(|| {
                let message = format!("Undefined symbol `{}`.", value);
                self.push_error(&message, location);
            })
    }

    fn resolve_expression_intconst(
        &mut self,
        value: &i128,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let str_value = format!("{}", value);
        let type_ = if str_value.parse::<u8>().is_ok() {
            Type::Char {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<i16>().is_ok() {
            Type::Short {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<u16>().is_ok() {
            Type::Short {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<i32>().is_ok() {
            Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<u32>().is_ok() {
            Type::Int {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<i64>().is_ok() {
            Type::Long {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<u64>().is_ok() {
            Type::Long {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            return Err(());
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_floatconst(
        &mut self,
        value: &f64,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let str_value = format!("{}", value);
        let type_ = if str_value.parse::<f32>().is_ok() {
            Type::Float {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else if str_value.parse::<f64>().is_ok() {
            Type::Double {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            }
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            return Err(());
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_charconst(&mut self) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Char {
            array_flag: false,
            array_len: None,
            pointer_flag: false,
            location: Location::default(),
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_strconst(&mut self) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Char {
            array_flag: false,
            array_len: None,
            pointer_flag: true,
            location: Location::default(),
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_prefix(
        &mut self,
        operator: &'static str,
        expression: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (expr_symbol, mut expr_type_bound) = self.resolve_expression(expression)?;
        match operator {
            "!" | "++" | "--" => {
                // num + ptr
                let symbol = self.symbol_table.make_expression_symbol();
                let type_bound = TypeBound::squeezed(Bound::Symbol(expr_symbol));
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            "+" | "-" => {
                // num
                expr_type_bound.upper.push(Bound::Type(Type::Double {
                    array_flag: false,
                    array_len: None,
                    pointer_flag: false,
                    location: Location::default(),
                }));
                expr_type_bound.upper.push(Bound::Type(Type::Char {
                    array_flag: false,
                    array_len: None,
                    pointer_flag: false,
                    location: Location::default(),
                }));
                self.symbol_table
                    .update_type_bound(expr_symbol.clone(), expr_type_bound);
                let symbol = self.symbol_table.make_expression_symbol();
                let type_bound = TypeBound::squeezed(Bound::Symbol(expr_symbol));
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            "*" | "&" => {
                // ptr
                let symbol = self.symbol_table.make_expression_symbol();
                let mut type_bound = TypeBound::squeezed(Bound::Symbol(expr_symbol));
                type_bound.upper.push(Bound::Type(Type::AnyRef));
                type_bound.lower.push(Bound::Type(Type::Null));
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            _ => unreachable!(),
        }
    }

    fn resolve_expression_infix(
        &mut self,
        left: &Expression,
        _operator: &'static str,
        right: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (_, left_type_bound) = self.resolve_expression(left)?;
        let (_, right_type_bound) = self.resolve_expression(right)?;
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::merge(&left_type_bound, &right_type_bound, "none");
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_suffix(
        &mut self,
        operator: &'static str,
        expression: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (expr_symbol, _) = self.resolve_expression(expression)?;
        match operator {
            "++" | "--" => {
                // num + ptr
                let symbol = self.symbol_table.make_expression_symbol();
                let type_bound = TypeBound::squeezed(Bound::Symbol(expr_symbol));
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            _ => unreachable!(),
        }
    }

    fn resolve_expression_group(
        &mut self,
        expression: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        self.resolve_expression(expression)
    }

    fn resolve_expression_index(
        &mut self,
        expression: &Expression,
        index: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (_, expr_type_bound) = self.resolve_expression(expression)?;
        let (index_symbol, mut index_type_bound) = self.resolve_expression(index)?;
        index_type_bound.upper.push(Bound::Type(Type::Long {
            signed_flag: false,
            array_flag: false,
            array_len: None,
            pointer_flag: false,
            location: Location::default(),
        }));
        index_type_bound.lower.push(Bound::Type(Type::Char {
            array_flag: false,
            array_len: None,
            pointer_flag: false,
            location: Location::default(),
        }));
        self.symbol_table
            .update_type_bound(index_symbol.clone(), index_type_bound.clone());
        let type_ = if let TypeBound {
            bounded: Some(type_),
            ..
        } = expr_type_bound
        {
            if type_.get_array().0 {
                type_.set_array(false, None)
            } else {
                let message = "Not an array.";
                self.push_error(&message, &expression.locate());
                return Err(());
            }
        } else {
            let message = "Array variable should be directly resolved.";
            self.push_error(&message, &expression.locate());
            return Err(());
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::squeezed(Bound::Type(type_));
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn resolve_expression_call(
        &mut self,
        expression: &Expression,
        arguments: &Vec<Expression>,
    ) -> Result<(Symbol, TypeBound), ()> {
        if let Expression::Ident { value, .. } = expression {
            let return_symbol = Symbol::Return {
                scope: self.symbol_table.current_scope,
                function: value.to_string(),
            };
            let return_type_bound = match self.symbol_table.func_returns.get(value) {
                Some(return_type_bound) => return_type_bound.clone(),
                None => {
                    let message = format!("Undefined function `{}`.", value);
                    self.push_error(&message, &expression.locate());
                    return Err(());
                }
            };
            self.symbol_table
                .define_symbol(return_symbol.clone(), return_type_bound.clone());
            let params = match self.symbol_table.func_params.get(value) {
                Some(params) => params.clone(),
                None => {
                    let message = format!("Undefined function `{}`.", value);
                    self.push_error(&message, &expression.locate());
                    return Err(());
                }
            };
            if arguments.len() != params.len() {
                let message = "Unequal arguments and parameters.";
                self.push_error(&message, &expression.locate());
                return Err(());
            }
            for (arg, (name, param_type_bound)) in arguments.iter().zip(params.iter()) {
                let (arg_symbol, mut arg_type_bound) = self.resolve_expression(arg)?;
                let param_symbol = Symbol::Parameter {
                    scope: self.symbol_table.current_scope,
                    function: value.to_string(),
                    name: name.to_string(),
                };
                let mut param_type_bound = param_type_bound.clone();
                arg_type_bound
                    .upper
                    .push(Bound::Symbol(param_symbol.clone()));
                param_type_bound
                    .lower
                    .push(Bound::Symbol(arg_symbol.clone()));
                self.symbol_table.define_symbol(arg_symbol, arg_type_bound);
                self.symbol_table
                    .define_symbol(param_symbol, param_type_bound);
            }
            let symbol = self.symbol_table.make_expression_symbol();
            let type_bound = TypeBound {
                upper: Vec::new(),
                lower: vec![Bound::Symbol(return_symbol)],
                bounded: None,
                wrapped: None,
            };
            self.symbol_table
                .define_symbol(symbol.clone(), type_bound.clone());
            Ok((symbol, type_bound))
        } else {
            let message = "Require a function name.";
            self.push_error(&message, &expression.locate());
            return Err(());
        }
    }

    fn resolve_expression_initlist(
        &mut self,
        pairs: &Vec<(Option<String>, Expression)>,
    ) -> Vec<(Option<String>, Result<(Symbol, TypeBound), ()>)> {
        pairs
            .iter()
            .map(|(name, expr)| (name.clone(), self.resolve_expression(expr)))
            .collect()
    }
}
