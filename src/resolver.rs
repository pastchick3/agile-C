use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::structure::{
    Array, Error, Expression, Function, Locate, Location, Pointer, Statement, StaticObject, Type,
    TypeRelationship,
};

#[derive(Hash, Eq, Clone, Debug)]
enum Symbol {
    Variable { name: String, param_flag: bool },
    Expression(String),
    Parameter { function: String, name: String },
    Return(String),
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        use Symbol::*;
        match (self, other) {
            (Variable { name: left, .. }, Variable { name: right, .. }) => left == right,
            (Expression(left), Expression(right)) => left == right,
            (
                Parameter {
                    function: func_left,
                    name: name_left,
                },
                Parameter {
                    function: func_right,
                    name: name_right,
                },
            ) => (func_left == func_right) && (name_left == name_right),
            (Return(left), Return(right)) => left == right,
            _ => false,
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
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            bounded: None,
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
}

#[derive(Debug)]
struct SymbolTable {
    current_func: Option<String>,
    func_returns: HashMap<String, TypeBound>,
    func_params: HashMap<String, IndexMap<String, TypeBound>>,
    structures: Vec<Vec<Type>>,
    symbols: Vec<HashMap<Symbol, TypeBound>>,
    expr_counter: Vec<usize>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            current_func: None,
            func_returns: HashMap::new(),
            func_params: HashMap::new(),
            structures: Vec::new(),
            symbols: Vec::new(),
            expr_counter: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.structures.push(Vec::new());
        self.symbols.push(HashMap::new());
        self.expr_counter.push(0);
    }

    fn leave_scope(&mut self) {
        self.structures.pop();
        self.symbols.pop();
        self.expr_counter.pop();
    }

    fn make_expression_symbol(&mut self) -> Symbol {
        let expr_count = self.expr_counter.last().unwrap();
        Symbol::Expression(expr_count.to_string())
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
        self.symbols.last_mut().unwrap().insert(symbol, type_bound);
    }

    fn define_structure(&mut self, structure: &Type) {
        if let Type::Struct { .. } = structure {
            self.structures.last_mut().unwrap().push(structure.clone());
        }
    }

    fn get_type_bound(&mut self, symbol: &Symbol) -> Option<TypeBound> {
        self.symbols
            .iter()
            .rev()
            .find_map(|symbols| symbols.get(symbol).cloned())
        // .or_else(|| {
        //     if let Symbol::Return(name) = symbol {
        //         let type_bound = self.func_returns.get(name).unwrap().clone();
        //         self.define_symbol(symbol.clone(), type_bound.clone());
        //         Some(type_bound)
        //     } else {
        //         None
        //     }
        // })
    }

    fn update_type_bound(&mut self, symbol: Symbol, type_bound: TypeBound) {
        self.symbols
            .iter_mut()
            .rev()
            .find(|symbols| symbols.contains_key(&symbol))
            .unwrap()
            .insert(symbol, type_bound);
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
        self.resolve_symbols();
        self.symbol_table.leave_scope();
        // check all functions (return/param) are resolved.
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
        let mut symbols = self.symbol_table.symbols.last().unwrap().clone();
        let mut modified = true;
        while modified {
            modified = false;
            for type_bound in symbols.values_mut() {
                self.resolve_type_bound(type_bound, &mut modified);
            }
        }
    }

    fn resolve_type_bound(&mut self, type_bound: &mut TypeBound, modified: &mut bool) {
        if type_bound.bounded.is_some() {
            return;
        }
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
                None => return,
                Some(type_) => match Type::compare_types(&mut upper_type, &type_) {
                    TypeRelationship::Sub => (),
                    TypeRelationship::Base => {
                        upper_type = type_;
                    }
                    TypeRelationship::Equal => (),
                    TypeRelationship::None => {
                        let message =
                            format!("Incompatible types `{}` and `{}`.", upper_type, type_);
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
                None => return,
                Some(type_) => match Type::compare_types(&mut lower_type, &type_) {
                    TypeRelationship::Sub => {
                        lower_type = type_;
                    }
                    TypeRelationship::Base => (),
                    TypeRelationship::Equal => (),
                    TypeRelationship::None => {
                        let message =
                            format!("Incompatible types `{}` and `{}`.", lower_type, type_);
                        self.push_error(&message, &type_.locate());
                        return;
                    }
                },
            }
        }
        let type_ = match Type::compare_types(&lower_type, &upper_type) {
            TypeRelationship::Sub | TypeRelationship::Equal => lower_type,
            _ => {
                let message = format!("Incompatible types `{}` and `{}`.", lower_type, upper_type);
                self.push_error(&message, &lower_type.locate());
                return;
            }
        };
        type_bound.bounded = Some(type_.clone());
        let TypeBound { wrapped, .. } = type_bound;
        let mut old_type = wrapped.as_ref().unwrap().borrow_mut();
        old_type.set_specialized(type_);
        *modified = true;
    }

    fn resolve_function(
        &mut self,
        Function {
            name,
            parameters,
            body,
            ..
        }: &Function,
    ) {
        self.symbol_table.current_func = Some(name.clone());
        self.symbol_table.enter_scope();
        for (name, type_) in parameters {
            let symbol = Symbol::Variable {
                name: name.clone(),
                param_flag: true,
            };
            let type_bound = TypeBound::new(Rc::clone(type_));
            self.symbol_table.define_symbol(symbol, type_bound);
        }
        self.resolve_statement(body);
        self.resolve_symbols();
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
            Statement::Return {
                expression,
                location,
            } => self.resolve_statement_return(expression, location),
            // Statement::Block {
            //     statements: Vec<Statement>,
            //     location: Location,
            // },
            // Statement::Def {
            //     base_type: Rc<RefCell<Type>>,
            //     declarators: Vec<(Rc<RefCell<Type>>, String, Option<Expression>)>,
            //     location: Location,
            // },
            // Statement::While {
            //     condition: Expression,
            //     body: Box<Statement>,
            //     location: Location,
            // },
            // Statement::Do {
            //     condition: Expression,
            //     body: Box<Statement>,
            //     location: Location,
            // },
            // Statement::For {
            //     initialization: Option<Box<Statement>>,
            //     condition: Option<Expression>,
            //     increment: Option<Expression>,
            //     body: Box<Statement>,
            //     location: Location,
            // },
            // Statement::If {
            //     condition: Expression,
            //     body: Box<Statement>,
            //     alternative: Option<Box<Statement>>,
            //     location: Location,
            // },
            // Statement::Switch {
            //     expression: Expression,
            //     branches: Vec<(Expression, Vec<Statement>)>,
            //     default: Option<Vec<Statement>>,
            //     location: Location,
            // },
            _ => unimplemented!(),
        }
    }

    fn resolve_statement_return(&mut self, expression: &Option<Expression>, location: &Location) {
        let current_func = self.symbol_table.current_func.as_ref().unwrap().to_string();
        let return_symbol = Symbol::Return(current_func.clone());
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

    fn resolve_expression(&mut self, expression: &Expression) -> Result<(Symbol, TypeBound), ()> {
        match expression {
            Expression::Ident { value, location } => self.resolve_expression_ident(value, location),
            Expression::IntConst { value, location } => {
                self.resolve_expression_intconst(value, location)
            }
            Expression::FloatConst { value, location } => {
                self.resolve_expression_floatconst(value, location)
            }
            // Expression::CharConst {
            //     value: String,
            //     location: Location,
            // },
            // Expression::StrConst {
            //     value: String,
            //     location: Location,
            // },
            // Expression::Prefix {
            //     operator: &'static str,
            //     expression: Box<Expression>,
            //     location: Location,
            // },
            // Expression::Infix {
            //     left: Box<Expression>,
            //     operator: &'static str,
            //     right: Box<Expression>,
            // },
            // Expression::Suffix {
            //     operator: &'static str,
            //     expression: Box<Expression>,
            // },
            // Expression::Group {
            //     expression: Box<Expression>,
            //     location: Location,
            // },
            // Expression::Index {
            //     expression: Box<Expression>,
            //     index: Box<Expression>,
            // },
            // Expression::Call {
            //     expression: Box<Expression>,
            //     arguments: Vec<Expression>,
            // },
            // Expression::InitList {
            //     pairs: Vec<(Option<String>, Expression)>,
            //     location: Location,
            // },
            _ => unimplemented!(),
        }
    }

    fn resolve_expression_ident(
        &mut self,
        value: &str,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let symbol = Symbol::Variable {
            name: value.to_string(),
            param_flag: true,
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
        let symbol = self.symbol_table.make_expression_symbol();
        let str_value = format!("{}", value);
        if str_value.parse::<u8>().is_ok() {
            let type_ = Type::Char {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<i16>().is_ok() {
            let type_ = Type::Short {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<u16>().is_ok() {
            let type_ = Type::Short {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<i32>().is_ok() {
            let type_ = Type::Int {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<u32>().is_ok() {
            let type_ = Type::Int {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<i64>().is_ok() {
            let type_ = Type::Long {
                signed_flag: true,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<u64>().is_ok() {
            let type_ = Type::Long {
                signed_flag: false,
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            Err(())
        }
    }

    fn resolve_expression_floatconst(
        &mut self,
        value: &f64,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let symbol = self.symbol_table.make_expression_symbol();
        let str_value = format!("{}", value);
        if str_value.parse::<f32>().is_ok() {
            let type_ = Type::Float {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else if str_value.parse::<f64>().is_ok() {
            let type_ = Type::Double {
                array_flag: false,
                array_len: None,
                pointer_flag: false,
                location: Location::default(),
            };
            Ok((symbol, TypeBound::bounded(type_)))
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            Err(())
        }
    }
}
