use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::structure::{
    Error, Expression, Function, Locate, Location, Statement, StaticObject, Type, TypeRelationship,
};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum Symbol {
    Variable {
        scope: Option<usize>,
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
    Type {
        refer: Type,
        location: Location
    },
    Symbol{
        refer: Symbol,
        location: Location
    },
}

#[derive(Clone, Debug)]
struct TypeBound {
    upper: Vec<Bound>,
    lower: Vec<Bound>,
    infix: Option<(Symbol, Symbol)>,
    member: Option<String>,
    ptr_mem: Option<String>,
    pointer: Option<bool>,
    bounded: Option<Type>,
    wrapped: Option<Rc<RefCell<Type>>>,
}

impl TypeBound {
    fn new() -> Self {

    }

    fn wrapped(wrapped: &Rc<RefCell<Type>>) -> Self {
        let type_ = Type::clone(&wrapped.borrow());
        let bounded = if let Type::T { .. } = type_ {
            None
        } else {
            Some(type_)
        };
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: None,
            member: None,
            ptr_member: None,
            reference: false,
            bounded,
            wrapped: Some(wrapped),
        }
    }

    fn bounded(bounded: &Type) -> TypeBound {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: None,
            member: None,
            ptr_member: None,
            reference: false,
            bounded: Some(bounded),
            wrapped: None,
        }
    }

    fn squeezed(bound: &Bound) -> TypeBound {
        TypeBound {
            upper: vec![bound.clone()],
            lower: vec![bound.clone()],
            infix: None,
            member: None,
            ptr_member: None,
            reference: false,
            bounded: None,
            wrapped: None,
        }
    }

    fn infixed(left: &Symbol, right: &Symbol) -> TypeBound {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: Some((left, right)),
            member: None,
            ptr_member: None,
            reference: false,
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
                TypeRelationship::Super => Some(left.clone()),
                TypeRelationship::Equal => Some(right.clone()),
                TypeRelationship::Invalid => None,
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
            infix: None,
            member: None,
            ptr_member: None,
            reference: false,
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
    fn new(index: usize, outer: Option<usize>) -> Self {
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
    fn new() -> Self {
        SymbolTable {
            current_func: None,
            current_scope: None,
            func_returns: HashMap::new(),
            func_params: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    // fn get_current_scope(&self) -> Option<&Scope> {
    //     let current = self.current_scope?;
    //     self.scopes.get(current)
    // }

    // fn get_scope(&self, index: Option<usize>) -> Option<&Scope> {
    //     self.scopes.get(index?)
    // }

    // fn get_current_scope_mut(&mut self) -> Option<&mut Scope> {
    //     let current = self.current_scope?;
    //     self.scopes.get_mut(current)
    // }

    // fn get_scope_mut(&mut self, index: Option<usize>) -> Option<&mut Scope> {
    //     self.scopes.get_mut(index?)
    // }

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

    // fn get_all_symbols(&self) -> Vec<(Symbol, TypeBound)> {
    //     self.scopes
    //         .iter()
    //         .map(|scope| scope.symbols.clone().into_iter())
    //         .flatten()
    //         .collect::<Vec<_>>()
    // }

    // fn update_global_symbols(&mut self) {
    //     let symbols = self.get_all_symbols();
    //     for (symbol, type_bound) in symbols.iter() {
    //         match symbol {
    //             Symbol::Parameter { function, name, .. } => {
    //                 let old_type_bound = self.func_params.get(function).unwrap().get(name).unwrap();
    //                 let type_bound = TypeBound::merge(old_type_bound, type_bound, "left");
    //                 self.func_params
    //                     .get_mut(function)
    //                     .unwrap()
    //                     .insert(name.to_string(), type_bound.clone());
    //             }
    //             Symbol::Return { function, .. } => {
    //                 let old_type_bound = self.func_returns.get(function).unwrap();
    //                 let type_bound = TypeBound::merge(old_type_bound, type_bound, "left");
    //                 self.func_returns
    //                     .insert(function.to_string(), type_bound.clone());
    //             }
    //             _ => (),
    //         }
    //     }
    //     for (symbol, _) in symbols.into_iter() {
    //         match &symbol {
    //             Symbol::Parameter { function, name, .. } => {
    //                 let type_bound = self
    //                     .func_params
    //                     .get(function)
    //                     .unwrap()
    //                     .get(name)
    //                     .unwrap()
    //                     .clone();
    //                 self.update_type_bound(symbol, type_bound);
    //             }
    //             Symbol::Return { function, .. } => {
    //                 let type_bound = self.func_returns.get(function).unwrap().clone();
    //                 self.update_type_bound(symbol, type_bound);
    //             }
    //             _ => (),
    //         }
    //     }
    // }

    fn update_wrapped(&mut self) {
        let symbols: Vec<_> = self
            .get_all_symbols()
            .into_iter()
            .map(|(_, type_bound)| type_bound)
            .collect();
        let returns = self.func_returns.values();
        let params = self
            .func_params
            .values()
            .map(|param| param.values())
            .flatten();
        let type_bounds = symbols.iter().chain(returns).chain(params);
        for type_bound in type_bounds {
            let TypeBound {
                wrapped, bounded, ..
            } = type_bound;
            if wrapped.is_some() && bounded.is_some() {
                let mut old_type = wrapped.as_ref().unwrap().borrow_mut();
                if !old_type.specialized() {
                    let bounded = match bounded.clone().unwrap() {
                        Type::Char {
                            num_flag: true,
                            array_flag,
                            array_len,
                            pointer_flag,
                            location,
                        } => Type::Short {
                            signed_flag: true,
                            array_flag,
                            array_len,
                            pointer_flag,
                            location,
                        },
                        type_ => type_,
                    };
                    let (array_flag, array_len) = old_type.get_array();
                    let bounded = bounded.set_array(array_flag, array_len);
                    old_type.set_specialized(bounded);
                }
            }
        }
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

    // fn implicit_return(&mut self) {
    //     for (_, type_bound) in self.symbol_table.func_returns.iter_mut() {
    //         if type_bound.upper.is_empty()
    //             && type_bound.lower.is_empty()
    //             && type_bound.bounded.is_none()
    //         {
    //             type_bound.bounded = Some(Type::Void {
    //                 array_flag: false,
    //                 array_len: None,
    //                 pointer_flag: false,
    //                 location: Location::default(),
    //             });
    //         }
    //     }
    // }

    // fn resolve_type_bound(
    //     &mut self,
    //     symbol: &Symbol,
    //     type_bound: &mut TypeBound,
    //     modified: &mut bool,
    //     aggressive: bool,
    // ) {
    //     let type_ = if let Some((left_symbol, right_symbol)) = &type_bound.infix {
    //         if type_bound.bounded.is_some() {
    //             return;
    //         }
    //         let mut _modified = false;
    //         let mut left_type_bound = self.symbol_table.get_type_bound(left_symbol).unwrap();

    //         self.resolve_type_bound(
    //             left_symbol,
    //             &mut left_type_bound,
    //             &mut _modified,
    //             aggressive,
    //         );
    //         let mut right_type_bound = self.symbol_table.get_type_bound(right_symbol).unwrap();
    //         self.resolve_type_bound(
    //             right_symbol,
    //             &mut right_type_bound,
    //             &mut _modified,
    //             aggressive,
    //         );
    //         if let Some(left_type) = left_type_bound.bounded {
    //             if let Some(right_type) = right_type_bound.bounded {
    //                 match Type::compare_types(&left_type, &right_type) {
    //                     TypeRelationship::Sub => right_type,
    //                     TypeRelationship::Base => left_type,
    //                     TypeRelationship::Equal => left_type,
    //                     TypeRelationship::None => {
    //                         let message = format!(
    //                             "Incompatible types `{:?}` and `{:?}`.",
    //                             left_type, right_type
    //                         );
    //                         self.push_error(&message, &left_type.locate());
    //                         return;
    //                     }
    //                 }
    //             } else {
    //                 return;
    //             }
    //         } else {
    //             return;
    //         }
    //     } else {
    //         let mut upper_type = Type::Any;
    //         for bound in type_bound.upper.iter() {
    //             let type_ = match bound {
    //                 Bound::Type(type_) => Some(type_).cloned(),
    //                 Bound::Symbol(symbol) => {
    //                     let type_ = self
    //                         .symbol_table
    //                         .get_type_bound(symbol)
    //                         .unwrap()
    //                         .bounded
    //                         .clone();

    //                     if type_bound.reference {
    //                         type_.map(|t| t.set_pointer_flag(true))
    //                     } else {
    //                         type_
    //                     }
    //                 }
    //                 Bound::Array(symbol) => self
    //                     .symbol_table
    //                     .get_type_bound(symbol)
    //                     .unwrap()
    //                     .bounded
    //                     .map(|type_| type_.set_array(false, None))
    //                     .clone(),
    //             };

    //             match type_ {
    //                 None => {
    //                     if !aggressive {
    //                         return;
    //                     }
    //                 }
    //                 Some(type_) => match Type::compare_types(&mut upper_type, &type_) {
    //                     TypeRelationship::Sub => (),
    //                     TypeRelationship::Base => {
    //                         upper_type = type_;
    //                     }
    //                     TypeRelationship::Equal => (),
    //                     TypeRelationship::None => {
    //                         let message =
    //                             format!("Incompatible types `{:?}` and `{:?}`.", upper_type, type_);
    //                         self.push_error(&message, &type_.locate());
    //                         return;
    //                     }
    //                 },
    //             }
    //         }

    //         let mut lower_type = Type::Nothing;
    //         for bound in type_bound.lower.iter() {
    //             let type_ = match bound {
    //                 Bound::Type(type_) => Some(type_).cloned(),
    //                 Bound::Symbol(symbol) => {
    //                     let type_ = self
    //                         .symbol_table
    //                         .get_type_bound(symbol)
    //                         .unwrap()
    //                         .bounded
    //                         .clone();

    //                     if type_bound.reference {
    //                         type_.map(|t| t.set_pointer_flag(true))
    //                     } else {
    //                         type_
    //                     }
    //                 }
    //                 Bound::Array(symbol) => self
    //                     .symbol_table
    //                     .get_type_bound(symbol)
    //                     .unwrap()
    //                     .bounded
    //                     .map(|type_| type_.set_array(false, None))
    //                     .clone(),
    //             };
    //             match type_ {
    //                 None => {
    //                     if !aggressive {
    //                         return;
    //                     }
    //                 }
    //                 Some(type_) => match Type::compare_types(&mut lower_type, &type_) {
    //                     TypeRelationship::Sub => {
    //                         lower_type = type_;
    //                     }
    //                     TypeRelationship::Base => (),
    //                     TypeRelationship::Equal => (),
    //                     TypeRelationship::None => {
    //                         let message =
    //                             format!("Incompatible types `{:?}` and `{:?}`.", lower_type, type_);
    //                         self.push_error(&message, &type_.locate());
    //                         return;
    //                     }
    //                 },
    //             }
    //         }

    //         let (lower_type, upper_type) =
    //             match Type::instantiate_any_nothing(lower_type.clone(), upper_type.clone()) {
    //                 Ok((lower_type, upper_type)) => (lower_type, upper_type),
    //                 Err(_) => {
    //                     return;
    //                 }
    //             };

    //         if let Some(old_type) = &type_bound.bounded {
    //             match Type::compare_types(old_type, &upper_type) {
    //                 TypeRelationship::Sub | TypeRelationship::Equal => {
    //                     match Type::compare_types(old_type, &lower_type) {
    //                         TypeRelationship::Base | TypeRelationship::Equal => {
    //                             return;
    //                         }
    //                         _ => {
    //                             let message = format!(
    //                                 "Incompatible types `{:?}` and `{:?}`.",
    //                                 old_type, lower_type
    //                             );
    //                             self.push_error(&message, &old_type.locate());
    //                             return;
    //                         }
    //                     }
    //                 }
    //                 _ => {
    //                     let message = format!(
    //                         "Incompatible types `{:?}` and `{:?}`.",
    //                         old_type, upper_type
    //                     );
    //                     self.push_error(&message, &old_type.locate());
    //                     return;
    //                 }
    //             }
    //         }
    //         match Type::compare_types(&lower_type, &upper_type) {
    //             TypeRelationship::Sub | TypeRelationship::Equal => lower_type,
    //             _ => {
    //                 let message = format!(
    //                     "Incompatible types `{:?}` and `{:?}`.",
    //                     lower_type, upper_type
    //                 );
    //                 self.push_error(&message, &lower_type.locate());
    //                 return;
    //             }
    //         }
    //     };
    //     // if type_bound.reference {
    //     //     type_bound.bounded.as_mut().map(|type_| type_.set_pointer_flag(true));
    //     // }
    //     // else
    //     if let Some(mem) = &type_bound.ptr_member {
    //         if let Type::Struct { members, .. } = &type_ {
    //             if let Type::Struct {
    //                 pointer_flag: true, ..
    //             } = &type_
    //             {
    //                 match members.get(mem) {
    //                     Some(type_) => {
    //                         type_bound.upper.clear();
    //                         type_bound.lower.clear();
    //                         type_bound.infix = None;
    //                         type_bound.member = None;
    //                         type_bound.bounded = Some(type_.clone());
    //                     }
    //                     None => {
    //                         let message = format!("Not a member.");
    //                         self.push_error(&message, &type_.locate());
    //                         return;
    //                     }
    //                 }
    //             } else {
    //                 let message = format!("Not a pointer to astructure.");
    //                 self.push_error(&message, &type_.locate());
    //                 return;
    //             }
    //         } else {
    //             let message = format!("Not a structure.");
    //             self.push_error(&message, &type_.locate());
    //             return;
    //         }
    //     } else if let Some(mem) = &type_bound.member {
    //         if let Type::Struct { members, .. } = &type_ {
    //             match members.get(mem) {
    //                 Some(type_) => {
    //                     type_bound.upper.clear();
    //                     type_bound.lower.clear();
    //                     type_bound.infix = None;
    //                     type_bound.member = None;
    //                     type_bound.bounded = Some(type_.clone());
    //                 }
    //                 None => {
    //                     let message = format!("Not a member.");
    //                     self.push_error(&message, &type_.locate());
    //                     return;
    //                 }
    //             }
    //         } else {
    //             let message = format!("Not a structure.");
    //             self.push_error(&message, &type_.locate());
    //             return;
    //         }
    //     } else {
    //         type_bound.bounded = Some(type_.clone());
    //     }

    //     self.symbol_table
    //         .update_type_bound(symbol.clone(), type_bound.clone());
    //     *modified = true;
    // }
}

/// A resolver that analyses a generic AST which may contain unknown
/// type information, infer these informaiton, and complete this AST.
pub struct Resolver<'a> {
    symbol_table: SymbolTable,
    generic_ast: Option<Vec<StaticObject>>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<StaticObject>, errors: &'a mut Vec<Error>) -> Self {
        Resolver {
            symbol_table: SymbolTable::new(),
            generic_ast: Some(generic_ast),
            errors,
        }
    }

    /// Resolve the generic AST to a complete AST.
    pub fn run(&mut self) -> Result<Vec<StaticObject>, ()> {
        // Analyse all functions to get type constrains.
        let ast: Vec<_> = self.generic_ast.take().unwrap();
        for obj in &ast {
            match obj {
                StaticObject::Type(structure) => self.symbol_table.define_structure(structure),
                StaticObject::Statement(_) => (),
                StaticObject::Function(function) => {
                    self.symbol_table.define_function(function);
                    self.analyse_function(function);
                }
            }
        }

        // Resolve types based on constraints and update type
        // information containted in AST.
        self.symbol_table.resolve_symbols();
        self.symbol_table.update_wrapped();

        if self.errors.is_empty() {
            Ok(ast)
        } else {
            Err(())
        }
    }

    /// A helper function to construct resolving errors.
    fn push_error(&mut self, message: &str, location: &Location) {
        self.errors.push(Error::Resolving {
            message: message.to_string(),
            location: location.clone(),
        });
    }

    fn analyse_function(
        &mut self,
        Function {
            return_type,
            name,
            parameters,
            body,
            ..
        }: &Function,
    ) {
        // Execute `symbol_table` prelude
        self.symbol_table.current_func = Some(name.clone());
        self.symbol_table.enter_scope();

        // Define function parameters.
        for (param, type_) in parameters {
            let symbol = Symbol::Parameter {
                function: name.clone(),
                scope: self.symbol_table.current_scope,
                name: param.clone(),
            };
            let type_bound = TypeBound::wrapped(type_);
            self.symbol_table.define_symbol(symbol, type_bound);
        }

        // Define the function return.
        let symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: name.clone(),
        };
        let type_bound = TypeBound::wrapped(return_type);
        self.symbol_table.define_symbol(symbol, type_bound);

        // Analyse the function body.
        self.analyse_statement(body);

        // Execute `symbol_table` postlude.
        self.symbol_table.leave_scope();
        self.symbol_table.current_func = None;
    }

    fn analyse_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Null(_) => (),
            Statement::Continue(_) => (),
            Statement::Break(_) => (),
            Statement::Include { .. } => (),
            Statement::Expr(expr) => match self.analyse_expression(expr) {
                _ => (),
            },
            Statement::Return { expression, location } => self.analyse_statement_return(expression, location),
            Statement::Block { statements, .. } => {
                for statement in statements {
                    self.analyse_statement(statement);
                }
            }
            Statement::Def {
                base_type,
                declarators,
                ..
            } => self.analyse_statement_def(base_type, declarators),
            Statement::While {
                condition, body, ..
            } => {
                match self.analyse_expression(condition) {
                    _ => (),
                }
                self.analyse_statement(body);
            }
            Statement::Do {
                condition, body, ..
            } => {
                match self.analyse_expression(condition) {
                    _ => (),
                }
                self.analyse_statement(body);
            }
            Statement::For {
                initialization,
                condition,
                increment,
                body,
                ..
            } => self.analyse_statement_for(initialization, condition, increment, body),
            Statement::If {
                condition,
                body,
                alternative,
                ..
            } => {
                match self.analyse_expression(condition) {
                    _ => (),
                }
                self.analyse_statement(body);
                if let Some(stmt) = alternative {
                    self.analyse_statement(stmt);
                }
            }
            Statement::Switch {
                expression,
                branches,
                default,
                ..
            } => self.analyse_statement_switch(expression, branches, default),
        }
    }

    fn analyse_statement_return(&mut self, expression: &Option<Expression>, location: &Location) {
        // Get the corresponding type bound.
        let current_func = self.symbol_table.current_func.clone().unwrap();
        let return_symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: current_func.clone(),
        };
        let mut return_type_bound = self.symbol_table.get_type_bound(&return_symbol).unwrap();

        // Add constraints.
        match expression {
            Some(expr) => {
                if let Ok((expr_symbol, mut expr_type_bound)) = self.analyse_expression(expr) {
                    return_type_bound
                        .lower
                        .push(Bound::Symbol{
                            refer: expr_symbol.clone(),
                            location: expr.locate(),
                        });
                    expr_type_bound
                        .upper
                        .push(Bound::Symbol{
                            refer: return_symbol.clone(),
                            location: expr.locate(),
                        });
                    self.symbol_table
                        .update_type_bound(return_symbol, return_type_bound);
                    self.symbol_table
                        .update_type_bound(expr_symbol, expr_type_bound);
                }
            }
            None => {
                let type_void = Type::Void(None);
                return_type_bound.upper.push(Bound::Type{
                    refer: type_void.clone(),
                    location: location.clone(),
                });
                return_type_bound.lower.push(Bound::Type{
                    refer: type_void.clone(),
                    location: location.clone(),
                });
            }
        }
    }

    fn analyse_statement_def(
        &mut self,
        base_type: &Rc<RefCell<Type>>,
        declarators: &Vec<(Rc<RefCell<Type>>, String, Option<Expression>)>,
    ) {
        if let Type::Struct {name, members,location} = base_type.borrow().clone() {
            // Define the structure.
            self.symbol_table.define_structure(&Type::Struct {name, members: members.clone(),location});

            for (type_, name, init) in declarators {
                // Define the variable.
                let symbol = Symbol::Variable {
                    scope: self.symbol_table.current_scope,
                    name: name.clone(),
                };
                let mut type_bound = TypeBound::wrapped(&type_);
                type_bound.upper.push(Bound::Type{
                    refer: type_.borrow().clone(),
                    location: type_.borrow().locate(),
                });
                type_bound.lower.push(Bound::Type{
                    refer: type_.borrow().clone(),
                    location: type_.borrow().locate(),
                });

                // Check the initializer.
                match init {
                    Some(Expression::InitList{initializers, location}) => {
                        let initializers: Vec<_> = initializers
                        .iter()
                        .map(|(name, expr)| (name, self.analyse_expression(expr))).collect();
                        for (field, expr) in initializers {
                            // Every field must be explicitly named.
                            if field.is_none() {
                                let message = "Unnamed fields in InitList.";
                                self.push_error(&message, location);
                                return;
                            }
                            let field = field.as_ref().unwrap();

                            // Add constraints for each field.
                            if expr.is_err() {
                                return;
                            }
                            let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                            match members.get(field) {
                                Some(type_) => {
                                    expr_type_bound.upper.push(Bound::Type{
                                        refer: type_.clone(),
                                        location: location.clone()
                                    })
                                }
                                None => {
                                    let message = format!("Unknown field `{}` in InitList.", field);
                                    self.push_error(&message, location);
                                    return;
                                }
                            }

                            // Update the type bound.
                            self.symbol_table
                                .update_type_bound(expr_symbol, expr_type_bound);
                        }
                    }
                    Some(expr) => {
                        let message = "Expect an init list.";
                        self.push_error(&message, &type_.borrow().locate());
                        return;
                    }
                    None => (),
                }

                // Update the type bound.
                self.symbol_table.update_type_bound(symbol, type_bound);
            }
        } else {
            // Find other variables.
            for (type_, name, init) in declarators {
                // Define the variable.
                let symbol = Symbol::Variable {
                    scope: self.symbol_table.current_scope,
                    name: name.clone(),
                };
                let mut type_bound = TypeBound::wrapped(type_);
                type_bound.upper.push(Bound::Type{
                    refer: type_.borrow().clone(),
                    location: type_.borrow().locate(),
                });
                type_bound.lower.push(Bound::Type{
                    refer: type_.borrow().clone(),
                    location: type_.borrow().locate(),
                });

                // Check the initializer.
                match init {
                    Some(Expression::InitList{initializers, location}) => {
                        // Find an array initializer.
                        let initializers: Vec<_> = initializers
                        .iter()
                        .map(|(name, expr)| (name, self.analyse_expression(expr))).collect();
                        for (field, expr) in initializers {
                            // An array initializer should not have named fields.
                            if field.is_some() {
                                let message = "Named fields in InitList.";
                                self.push_error(&message, location);
                                return;
                            }

                            // Add constraints.
                            if expr.is_err() {
                                return;
                            }
                            let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                            type_bound.lower.push(Bound::Symbol{
                                refer: expr_symbol.clone(),
                                location: location.clone()
                            });
                            expr_type_bound.upper.push(Bound::Symbol{
                                refer: symbol.clone(),
                                location: location.clone()
                            });

                            // Update the type bound.
                            self.symbol_table
                                .update_type_bound(expr_symbol, expr_type_bound);
                        }
                    }
                    Some(expr) => {
                        // Find other initializers.
                        let location = expr.locate();

                        // Add constraints.
                        let expr = self.analyse_expression(expr);
                        if expr.is_err() {
                            return;
                        }
                        let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                        type_bound.lower.push(Bound::Symbol{
                            refer: expr_symbol.clone(),
                            location: location.clone()
                        });
                        expr_type_bound.upper.push(Bound::Symbol{
                            refer: symbol.clone(),
                            location: location.clone()
                        });

                        // Update the type bound.
                        self.symbol_table
                        .update_type_bound(expr_symbol, expr_type_bound);
                    }
                    None => (),
                }

                // Update the type bound.
                self.symbol_table.update_type_bound(symbol, type_bound);
            }
        }
    }

    fn analyse_statement_for(
        &mut self,
        initialization: &Option<Box<Statement>>,
        condition: &Option<Expression>,
        increment: &Option<Expression>,
        body: &Statement,
    ) {
        self.symbol_table.enter_scope();
        if let Some(stmt) = initialization {
            self.analyse_statement(stmt);
        }
        if let Some(expr) = condition {
            match self.analyse_expression(expr) {
                _ => (),
            }
        }
        if let Some(expr) = increment {
            match self.analyse_expression(expr) {
                _ => (),
            }
        }
        self.analyse_statement(body);
        self.symbol_table.leave_scope();
    }

    fn analyse_statement_switch(
        &mut self,
        expression: &Expression,
        branches: &Vec<(Expression, Vec<Statement>)>,
        default: &Option<Vec<Statement>>,
    ) {
        match self.analyse_expression(expression) {
            _ => (),
        };
        for (expr, stmts) in branches {
            match self.analyse_expression(expr) {
                _ => (),
            };
            for stmt in stmts {
                self.analyse_statement(stmt);
            }
        }
        if let Some(stmts) = default {
            for stmt in stmts {
                self.analyse_statement(stmt);
            }
        }
    }

    fn analyse_expression(&mut self, expression: &Expression) -> Result<(Symbol, TypeBound), ()> {
        match expression {
            Expression::Ident { value, location } => self.analyse_expression_ident(value, location),
            Expression::IntConst { value, location } => {
                self.analyse_expression_intconst(value, location)
            }
            Expression::FloatConst { value, location } => {
                self.analyse_expression_floatconst(value, location)
            }
            Expression::CharConst { .. } => self.analyse_expression_charconst(),
            Expression::StrConst { .. } => self.analyse_expression_strconst(),
            Expression::Prefix {
                operator,
                expression,
                location,
            } => self.analyse_expression_prefix(operator, expression, location),
            Expression::Infix {
                left,
                operator,
                right,
            } => self.analyse_expression_infix(left, operator, right),
            Expression::Postfix {
                operator,
                expression,
            } => self.analyse_expression_postfix(operator, expression),
            Expression::Group { expression, .. } => self.analyse_expression(expression),
            Expression::Index { expression, index } => {
                self.analyse_expression_index(expression, index)
            }
            Expression::Call {
                expression,
                arguments,
            } => self.analyse_expression_call(expression, arguments),
            Expression::InitList { location, .. } => {
                let message = "Unexpect InitList.";
                self.push_error(&message, location);
                Err(())
            }
        }
    }

    fn analyse_expression_ident(
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
                scope: self.symbol_table.current_scope,
                name: value.to_string(),
            }
        };
        self.symbol_table
            .get_type_bound(&symbol)
            .map(|type_bound| (symbol, type_bound))
            .ok_or_else(|| {
                let message = format!("Undefined variable `{}`.", value);
                self.push_error(&message, location);
            })
    }

    fn analyse_expression_intconst(
        &mut self,
        value: &i128,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let str_value = format!("{}", value);
        let type_ = if str_value.parse::<u8>().is_ok() {
            Type::Byte
        } else if str_value.parse::<i16>().is_ok() {
            Type::Short(None)
        } else if str_value.parse::<u16>().is_ok() {
            Type::UnsignedShort(None)
        } else if str_value.parse::<i32>().is_ok() {
            Type::Int(None)
        } else if str_value.parse::<u32>().is_ok() {
            Type::UnsignedInt(None)
        } else if str_value.parse::<i64>().is_ok() {
            Type::Long(None)
        } else if str_value.parse::<u64>().is_ok() {
            Type::UnsignedLong(None)
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            return Err(());
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_floatconst(
        &mut self,
        value: &f64,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let str_value = format!("{}", value);
        let type_ = if str_value.parse::<f32>().is_ok() {
            Type::Float(None)
        } else if str_value.parse::<f64>().is_ok() {
            Type::Double(None)
        } else {
            let message = format!("Range error for `{}`.", value);
            self.push_error(&message, location);
            return Err(());
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_charconst(&mut self) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Char(None);
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_strconst(&mut self) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Pointer {
            refer: Box::new(Type::Char(None)),
            location: None,
        };
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_prefix(
        &mut self,
        operator: &'static str,
        expression: &Expression,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (expr_symbol, mut expr_type_bound) = self.analyse_expression(expression)?;
        match operator {
            "!" | "++" | "--" => {
                // Allow any types.
                Ok((expr_symbol, expr_type_bound))
            }
            "+" | "-" => {
                // Allow numerical types.
                expr_type_bound.upper.push(Bound::Type{
                    refer: Type::Double(None),
                    location: location.clone()
                });
                expr_type_bound.lower.push(Bound::Type{
                    refer: Type::Byte,
                    location: location.clone()
                });
                let symbol = self.symbol_table.make_expression_symbol();
                let type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: expr_symbol.clone(),
                    location: location.clone(),
                });
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                self.symbol_table
                    .update_type_bound(expr_symbol, expr_type_bound);
                Ok((symbol, type_bound))
            }
            "&" => {
                // Allow any types.
                // Set `TypeBound::pointer`.
                let symbol = self.symbol_table.make_expression_symbol();
                let mut type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: expr_symbol.clone(),
                    location: location.clone(),
                });
                type_bound.pointer = Some(true);
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            "*" => {
                // Allow pointer types.
                // Set `TypeBound::pointer`.
                let symbol = self.symbol_table.make_expression_symbol();
                let mut type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: expr_symbol.clone(),
                    location: location.clone(),
                });
                type_bound.pointer = Some(false);
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            _ => unreachable!(),
        }
    }

    fn analyse_expression_infix(
        &mut self,
        left: &Expression,
        operator: &'static str,
        right: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (left_symbol, left_type_bound) = self.analyse_expression(left)?;
        let (right_symbol, right_type_bound) = self.analyse_expression(right)?;

        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = match operator {
            "+" | "-" | "*" | "/" | "%"  => {
                // Allow any types.
                // Set `TypeBound::infix`.
                TypeBound::infixed(&left_symbol, &right_symbol)
            }
            "=" | "+=" | "-=" | "*=" | "/=" | "%=" => {
                // Allow any types.
                // return the left type bound.
                left_type_bound
            }
            "<" | ">" | "<=" | ">=" | "=="| "!="| "&&"| "||" => {
                // Allow any types.
                // return int.
                let type_ = Type::Int(None);
                let type_bound = TypeBound::bounded(&type_);
                type_bound
            }
            "." => {
                // Allow any types.
                // Set `TypeBound::member`.
                let name = if let Expression::Ident { value, .. } = right {
                    value
                } else {
                    let message = format!("Struct member should be a string.");
                    self.push_error(&message, &left.locate());
                    return Err(());
                };
                let type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: left_symbol.clone(),
                    location: left.locate(),
                });
                type_bound.member = Some(name.to_string());
                type_bound
            }
            "->" => {
                // Allow any types.
                // Set `TypeBound::ptr_mem`.
                let name = if let Expression::Ident { value, .. } = right {
                    value
                } else {
                    let message = format!("Struct member should be a string.");
                    self.push_error(&message, &left.locate());
                    return Err(());
                };
                let type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: left_symbol.clone(),
                    location: left.locate(),
                });
                type_bound.ptr_mem = Some(name.to_string());
                type_bound
            }
            _ => unreachable!(),
        };

        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_postfix(
        &mut self,
        operator: &'static str,
        expression: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (expr_symbol, _) = self.analyse_expression(expression)?;
        match operator {
            "++" | "--" => {
                // Allow any types.
                let symbol = self.symbol_table.make_expression_symbol();
                let type_bound = TypeBound::squeezed(&Bound::Symbol{
                    refer: expr_symbol.clone(),
                    location: expression.locate(),
                });
                self.symbol_table
                    .define_symbol(symbol.clone(), type_bound.clone());
                Ok((symbol, type_bound))
            }
            _ => unreachable!(),
        }
    }

    fn analyse_expression_index(
        &mut self,
        expression: &Expression,
        index: &Expression,
    ) -> Result<(Symbol, TypeBound), ()> {
        let (expr_symbol, expr_type_bound) = self.analyse_expression(expression)?;
        let (index_symbol, index_type_bound) = self.analyse_expression(index)?;

        // index must be integral types.
        index_type_bound.upper.push(Bound::Type{
            refer: Type::Long(None),
            location: index.locate(),
        });
        index_type_bound.lower.push(Bound::Type{
            refer: Type::Byte,
            location: index.locate(),
        });
        self.symbol_table
            .update_type_bound(index_symbol.clone(), index_type_bound.clone());

        // Require the expression to be a pointer.
        let symbol = self.symbol_table.make_expression_symbol();
        let type_bound = TypeBound::squeezed(&Bound::Symbol{
            refer: expr_symbol.clone(),
            location: expression.locate(),
        });
        type_bound.pointer = Some(true);
        self.symbol_table
            .define_symbol(symbol.clone(), type_bound.clone());
        Ok((symbol, type_bound))
    }

    fn analyse_expression_call(
        &mut self,
        expression: &Expression,
        arguments: &Vec<Expression>,
    ) -> Result<(Symbol, TypeBound), ()> {
        if let Expression::Ident { value, .. } = expression {
            // Define the function return symbol.
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
            
            // Constrain arguments and parameters.
            let params = match self.symbol_table.func_params.get(value) {
                Some(params) => params.clone(),
                None => {
                    let message = format!("Undefined function `{}`.", value);
                    self.push_error(&message, &expression.locate());
                    return Err(());
                }
            };
            if arguments.len() != params.len() {
                let message = "Unequal lengths of arguments and parameters.";
                self.push_error(&message, &expression.locate());
                return Err(());
            }
            for (arg, (param, param_type_bound)) in arguments.iter().zip(params.into_iter()) {
                let (arg_symbol, arg_type_bound) = self.analyse_expression(arg)?;
                let param_symbol = Symbol::Parameter {
                    scope: self.symbol_table.current_scope,
                    function: value.to_string(),
                    name: param.to_string(),
                };
                arg_type_bound
                    .upper
                    .push(Bound::Symbol{
                        refer: param_symbol.clone(),
                        location: arg.locate(),
                    });
                param_type_bound
                    .lower
                    .push(Bound::Symbol{
                        refer: arg_symbol.clone(),
                        location: arg.locate(),
                    });
                self.symbol_table.define_symbol(arg_symbol, arg_type_bound);
                self.symbol_table
                    .define_symbol(param_symbol, param_type_bound);
            }

            // Set up the function call symbol.
            let symbol = self.symbol_table.make_expression_symbol();
            let type_bound = TypeBound::new();
            type_bound.lower
                    .push(Bound::Symbol{
                        refer: return_symbol.clone(),
                        location: expression.locate(),
                    });
            self.symbol_table
                .define_symbol(symbol.clone(), type_bound.clone());
            Ok((symbol, type_bound))
        } else {
            // Require the function name directly available.
            let message = "Require a function name.";
            self.push_error(&message, &expression.locate());
            return Err(());
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::lexer::Lexer;
//     use crate::parser::Parser;
//     use crate::preprocessor::Preprocessor;

//     #[test]
//     fn function() {
//         let source = "
//             f(a) {
//                 return a;
//             }

//             m() {
//                 f(1);
//                 return;
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![
//             StaticObject::Function(Box::new(Function {
//                 return_type: Rc::new(RefCell::new(Type::T {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     specialized: Some(Box::new(Type::Short {
//                         signed_flag: true,
//                         array_flag: false,
//                         array_len: None,
//                         pointer_flag: false,
//                         location: Location::default(),
//                     })),
//                     location: Location::default(),
//                 })),
//                 name: "f".to_string(),
//                 parameters: [(
//                     "a".to_string(),
//                     Rc::new(RefCell::new(Type::T {
//                         array_flag: false,
//                         array_len: None,
//                         pointer_flag: false,
//                         specialized: Some(Box::new(Type::Short {
//                             signed_flag: true,
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                         })),
//                         location: Location::default(),
//                     })),
//                 )]
//                 .iter()
//                 .cloned()
//                 .collect(),
//                 body: Statement::Block {
//                     statements: vec![Statement::Return {
//                         expression: Some(Expression::Ident {
//                             value: "a".to_string(),
//                             location: Location::default(),
//                         }),
//                         location: Location::default(),
//                     }],
//                     location: Location::default(),
//                 },
//                 location: Location::default(),
//             })),
//             StaticObject::Function(Box::new(Function {
//                 return_type: Rc::new(RefCell::new(Type::T {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     specialized: Some(Box::new(Type::Void {
//                         array_flag: false,
//                         array_len: None,
//                         pointer_flag: false,
//                         location: Location::default(),
//                     })),
//                     location: Location::default(),
//                 })),
//                 name: "m".to_string(),
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![
//                         Statement::Expr(Expression::Call {
//                             expression: Box::new(Expression::Ident {
//                                 value: "f".to_string(),
//                                 location: Location::default(),
//                             }),
//                             arguments: vec![Expression::IntConst {
//                                 value: 1,
//                                 location: Location::default(),
//                             }],
//                         }),
//                         Statement::Return {
//                             expression: None,
//                             location: Location::default(),
//                         },
//                     ],
//                     location: Location::default(),
//                 },
//                 location: Location::default(),
//             })),
//         ];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn function_reverse() {
//         let source = "
//             f() {
//                 a = 1;
//                 return a;
//             }

//             int m() {
//                 return f();
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![
//             StaticObject::Function(Box::new(Function {
//                 return_type: Rc::new(RefCell::new(Type::T {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     specialized: Some(Box::new(Type::Short {
//                         signed_flag: true,
//                         array_flag: false,
//                         array_len: None,
//                         pointer_flag: false,
//                         location: Location::default(),
//                     })),
//                     location: Location::default(),
//                 })),
//                 name: "f".to_string(),
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![
//                         Statement::Def {
//                             base_type: Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 location: Location::default(),
//                                 specialized: None,
//                             })),
//                             declarators: vec![(
//                                 Rc::new(RefCell::new(Type::T {
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     specialized: Some(Box::new(Type::Short {
//                                         signed_flag: true,
//                                         array_flag: false,
//                                         array_len: None,
//                                         pointer_flag: false,
//                                         location: Location::default(),
//                                     })),
//                                     location: Location::default(),
//                                 })),
//                                 "a".to_string(),
//                                 Some(Expression::IntConst {
//                                     value: 1,
//                                     location: Location::default(),
//                                 }),
//                             )],
//                             location: Location::default(),
//                         },
//                         Statement::Return {
//                             expression: Some(Expression::Ident {
//                                 value: "a".to_string(),
//                                 location: Location::default(),
//                             }),
//                             location: Location::default(),
//                         },
//                     ],
//                     location: Location::default(),
//                 },
//                 location: Location::default(),
//             })),
//             StaticObject::Function(Box::new(Function {
//                 return_type: Rc::new(RefCell::new(Type::Int {
//                     signed_flag: true,
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::default(),
//                 })),
//                 name: "m".to_string(),
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![Statement::Return {
//                         expression: Some(Expression::Call {
//                             expression: Box::new(Expression::Ident {
//                                 value: "f".to_string(),
//                                 location: Location::default(),
//                             }),
//                             arguments: Vec::new(),
//                         }),
//                         location: Location::default(),
//                     }],
//                     location: Location::default(),
//                 },
//                 location: Location::default(),
//             })),
//         ];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn statement_for() {
//         let source = "
//             void f() {
//                 for (i = 1; ; ) ;
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![StaticObject::Function(Box::new(Function {
//             return_type: Rc::new(RefCell::new(Type::Void {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 location: Location::default(),
//             })),
//             name: "f".to_string(),
//             parameters: IndexMap::new(),
//             body: Statement::Block {
//                 statements: vec![Statement::For {
//                     initialization: Some(Box::new(Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Short {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "i".to_string(),
//                             Some(Expression::IntConst {
//                                 value: 1,
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     })),
//                     condition: None,
//                     increment: None,
//                     body: Box::new(Statement::Null(Location::default())),
//                     location: Location::default(),
//                 }],
//                 location: Location::default(),
//             },
//             location: Location::default(),
//         }))];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn expression_const() {
//         let source = "
//             void f() {
//                 a = 1;
//                 b = 1.1;
//                 c = 'c';
//                 d = \"d\";
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![StaticObject::Function(Box::new(Function {
//             return_type: Rc::new(RefCell::new(Type::Void {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 location: Location::default(),
//             })),
//             name: "f".to_string(),
//             parameters: IndexMap::new(),
//             body: Statement::Block {
//                 statements: vec![
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Short {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "a".to_string(),
//                             Some(Expression::IntConst {
//                                 value: 1,
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Float {
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "b".to_string(),
//                             Some(Expression::FloatConst {
//                                 value: 1.1,
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Char {
//                                     num_flag: false,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "c".to_string(),
//                             Some(Expression::CharConst {
//                                 value: "'c'".to_string(),
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Char {
//                                     num_flag: false,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: true,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "d".to_string(),
//                             Some(Expression::StrConst {
//                                 value: "\"d\"".to_string(),
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                 ],
//                 location: Location::default(),
//             },
//             location: Location::default(),
//         }))];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn expression_general() {
//         let source = "
//             f() {
//                 a = +1;
//                 b = a++;
//                 return a + b;
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![StaticObject::Function(Box::new(Function {
//             return_type: Rc::new(RefCell::new(Type::T {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 specialized: Some(Box::new(Type::Short {
//                     signed_flag: true,
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::default(),
//                 })),
//                 location: Location::default(),
//             })),
//             name: "f".to_string(),
//             parameters: IndexMap::new(),
//             body: Statement::Block {
//                 statements: vec![
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Short {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "a".to_string(),
//                             Some(Expression::Prefix {
//                                 expression: Box::new(Expression::IntConst {
//                                     value: 1,
//                                     location: Location::default(),
//                                 }),
//                                 operator: "+",
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: false,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Short {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "b".to_string(),
//                             Some(Expression::Suffix {
//                                 expression: Box::new(Expression::Ident {
//                                     value: "a".to_string(),
//                                     location: Location::default(),
//                                 }),
//                                 operator: "++",
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Return {
//                         expression: Some(Expression::Infix {
//                             left: Box::new(Expression::Ident {
//                                 value: "a".to_string(),
//                                 location: Location::default(),
//                             }),
//                             operator: "+",
//                             right: Box::new(Expression::Ident {
//                                 value: "b".to_string(),
//                                 location: Location::default(),
//                             }),
//                         }),
//                         location: Location::default(),
//                     },
//                 ],
//                 location: Location::default(),
//             },
//             location: Location::default(),
//         }))];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn array() {
//         let source = "
//             f() {
//                 T a[] = {1};
//                 return a[0];
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![StaticObject::Function(Box::new(Function {
//             return_type: Rc::new(RefCell::new(Type::T {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 specialized: Some(Box::new(Type::Short {
//                     signed_flag: true,
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::default(),
//                 })),
//                 location: Location::default(),
//             })),
//             name: "f".to_string(),
//             parameters: IndexMap::new(),
//             body: Statement::Block {
//                 statements: vec![
//                     Statement::Def {
//                         base_type: Rc::new(RefCell::new(Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::default(),
//                             specialized: None,
//                         })),
//                         declarators: vec![(
//                             Rc::new(RefCell::new(Type::T {
//                                 array_flag: true,
//                                 array_len: None,
//                                 pointer_flag: false,
//                                 specialized: Some(Box::new(Type::Short {
//                                     signed_flag: true,
//                                     array_flag: true,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::default(),
//                                 })),
//                                 location: Location::default(),
//                             })),
//                             "a".to_string(),
//                             Some(Expression::InitList {
//                                 pairs: vec![(
//                                     None,
//                                     Expression::IntConst {
//                                         value: 1,
//                                         location: Location::default(),
//                                     },
//                                 )],
//                                 location: Location::default(),
//                             }),
//                         )],
//                         location: Location::default(),
//                     },
//                     Statement::Return {
//                         expression: Some(Expression::Index {
//                             expression: Box::new(Expression::Ident {
//                                 value: "a".to_string(),
//                                 location: Location::default(),
//                             }),
//                             index: Box::new(Expression::IntConst {
//                                 value: 0,
//                                 location: Location::default(),
//                             }),
//                         }),
//                         location: Location::default(),
//                     },
//                 ],
//                 location: Location::default(),
//             },
//             location: Location::default(),
//         }))];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn implicit_return() {
//         let source = "
//             f() {}
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![StaticObject::Function(Box::new(Function {
//             return_type: Rc::new(RefCell::new(Type::T {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 specialized: Some(Box::new(Type::Void {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::default(),
//                 })),
//                 location: Location::default(),
//             })),
//             name: "f".to_string(),
//             parameters: IndexMap::new(),
//             body: Statement::Block {
//                 statements: Vec::new(),
//                 location: Location::default(),
//             },
//             location: Location::default(),
//         }))];
//         let mut errors = Vec::new();
//         let lines = Preprocessor::new("file", source, &mut errors)
//             .run()
//             .unwrap();
//         let tokens = Lexer::new(lines, &mut errors).run().unwrap();
//         let generic_ast = Parser::new(tokens, &mut errors).run().unwrap();
//         let ast = Resolver::new(generic_ast, &mut errors).run().unwrap();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }
// }
