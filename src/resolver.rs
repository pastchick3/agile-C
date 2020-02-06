use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::structure::{
    Error, Expression, Function, Locate, Location, Statement, StaticObject, Type, TypeRelationship,
};

/// Every symbol will be mapped to a type bound.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Symbol {
    Variable {
        scope: usize,
        name: String,
        location: Location,
    },
    Expression {
        scope: usize,
        count: usize,
        location: Location,
    },
    Parameter {
        scope: usize,
        function: String,
        name: String,
    },
    Return {
        scope: usize,
        function: String,
    },
}

impl Symbol {
    fn get_scope(&self) -> usize {
        match self {
            Symbol::Variable { scope, .. } => scope.clone(),
            Symbol::Expression { scope, .. } => scope.clone(),
            Symbol::Parameter { scope, .. } => scope.clone(),
            Symbol::Return { scope, .. } => scope.clone(),
        }
    }

    fn set_scope(&mut self, s: usize) {
        match self {
            Symbol::Variable { scope, .. } => *scope = s,
            Symbol::Expression { scope, .. } => *scope = s,
            Symbol::Parameter { scope, .. } => *scope = s,
            Symbol::Return { scope, .. } => *scope = s,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable { name, location, .. } => write!(f, "{} at {}", name, location),
            Symbol::Expression { location, .. } => write!(f, "Expr at {}", location),
            Symbol::Parameter { function, name, .. } => {
                write!(f, "Param `{}` of `{}`", name, function)
            }
            Symbol::Return { function, .. } => write!(f, "Return of `{}`", function),
        }
    }
}

/// A symbol can be bounded by either a concrete type or another symbol.
#[derive(Clone, Debug)]
enum Bound {
    Type {
        refer: Type,
        pointer: Option<bool>, // pointers, true for `&`, false for `*`
        location: Location,
    },
    Symbol {
        refer: Symbol,
        pointer: Option<bool>, // pointers, true for `&`, false for `*`
        location: Location,
    },
}

/// Every symbol will be mapped to a type bound.
#[derive(Clone, Debug)]
struct TypeBound {
    upper: Vec<Bound>,                  // upper bounds
    lower: Vec<Bound>,                  // lower bounds
    infix: Option<(Symbol, Symbol)>,    // infix expressions (`Option<(left, right)>`)
    member: Option<String>,             // structure members using dot operators (`Option<member>`)
    ptr_mem: Option<String>, // structure members using arrow operators (`Option<member>`)
    bounded: Option<Type>,   // concrete types that are determined
    wrapped: Option<Rc<RefCell<Type>>>, // types in AST that are wrapped
}

impl TypeBound {
    fn new() -> Self {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: None,
            member: None,
            ptr_mem: None,
            bounded: None,
            wrapped: None,
        }
    }

    fn wrapped(wrapped: &Rc<RefCell<Type>>) -> Self {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: None,
            member: None,
            ptr_mem: None,
            bounded: None,
            wrapped: Some(Rc::clone(wrapped)),
        }
    }

    fn bounded(bounded: &Type) -> TypeBound {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: None,
            member: None,
            ptr_mem: None,
            bounded: Some(bounded.clone()),
            wrapped: None,
        }
    }

    fn squeezed(bound: &Bound) -> TypeBound {
        TypeBound {
            upper: vec![bound.clone()],
            lower: vec![bound.clone()],
            infix: None,
            member: None,
            ptr_mem: None,
            bounded: None,
            wrapped: None,
        }
    }

    fn infixed(left: &Symbol, right: &Symbol) -> TypeBound {
        TypeBound {
            upper: Vec::new(),
            lower: Vec::new(),
            infix: Some((left.clone(), right.clone())),
            member: None,
            ptr_mem: None,
            bounded: None,
            wrapped: None,
        }
    }

    /// Merge two type bounds.
    /// It is only used once to merge function symbols, so we
    /// assume only `upper` and `lower` fields are relevant, and
    /// we will use left type bound's `wrapped` field.
    fn merge(left: &TypeBound, right: &TypeBound) -> TypeBound {
        let mut upper = left.upper.clone();
        upper.append(&mut right.upper.clone());
        let mut lower = left.lower.clone();
        lower.append(&mut right.lower.clone());
        TypeBound {
            upper,
            lower,
            infix: None,
            member: None,
            ptr_mem: None,
            bounded: None,
            wrapped: left.wrapped.clone(),
        }
    }
}

/// A structure containing type information in each scope.
#[derive(Debug)]
struct Scope {
    index: usize,         // the id of a scope
    outer: Option<usize>, // the id of its enclosing scope
    symbols: HashMap<Symbol, TypeBound>,
    expr_counter: usize,   // used to define expression symbols
    structures: Vec<Type>, // structures defined in this scope
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

// A structure containing all scopes and information about functions.
#[derive(Debug)]
struct SymbolTable {
    current_func: Option<String>,
    current_scope: usize,
    func_returns: HashMap<String, TypeBound>,
    func_params: HashMap<String, IndexMap<String, TypeBound>>,
    scopes: Vec<Scope>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            current_func: None,
            current_scope: 0,
            func_returns: HashMap::new(),
            func_params: HashMap::new(),
            scopes: vec![Scope::new(0, None)],
        }
    }

    /// Enter a new scope.
    fn enter_scope(&mut self) {
        let outer = self.current_scope;
        let current = self.scopes.len();
        self.current_scope = current;
        self.scopes.push(Scope::new(current, Some(outer)));
    }

    /// Leave the current scope.
    fn leave_scope(&mut self) {
        self.current_scope = self.scopes[self.current_scope]
            .outer
            .expect("Try to leave the top scope.");
    }

    /// Test whether a name stands for a function parameter
    /// in the current scope.
    fn is_parameter(&self, name: &str) -> bool {
        let current_func = self.current_func.as_ref().unwrap();
        self.func_params
            .get(current_func)
            .unwrap()
            .contains_key(name)
    }

    /// Create an expression symbol in the current scope.
    fn make_expression_symbol(&mut self, location: &Location) -> Symbol {
        let scope = &mut self.scopes[self.current_scope];
        let symbol = Symbol::Expression {
            scope: self.current_scope,
            count: scope.expr_counter,
            location: location.clone(),
        };
        scope.expr_counter += 1;
        symbol
    }

    /// Define a structure in the current scope.
    fn define_structure(&mut self, structure: &Type) {
        let scope = &mut self.scopes[self.current_scope];
        if let Type::Struct { .. } = structure {
            scope.structures.push(structure.clone());
        }
    }

    /// Define a function in the top scope.
    fn define_function(&mut self, function: &Function) {
        let Function {
            return_type,
            name,
            parameters,
            ..
        } = function;
        self.func_returns
            .insert(name.clone(), TypeBound::wrapped(return_type));
        let parameters: IndexMap<_, _> = parameters
            .iter()
            .map(|(param, type_)| (param.clone(), TypeBound::wrapped(type_)))
            .collect();
        self.func_params.insert(name.clone(), parameters);
    }

    /// Define a symbol in the current scope.
    fn define_symbol(&mut self, symbol: &Symbol, type_bound: &TypeBound) {
        let scope = &mut self.scopes[self.current_scope];
        scope.symbols.insert(symbol.clone(), type_bound.clone());
    }

    /// Get the corresponding type bound of the given symbol in scopes.
    fn get_local_type_bound(&self, symbol: &mut Symbol) -> Option<TypeBound> {
        let mut scope = &self.scopes[symbol.get_scope()];
        loop {
            if let Some(type_bound) = scope.symbols.get(symbol) {
                return Some(type_bound.clone());
            } else if let Some(outer) = scope.outer {
                scope = &self.scopes[outer];
                symbol.set_scope(symbol.get_scope() - 1);
            } else {
                return None;
            }
        }
    }

    /// Get the corresponding type bound of the given symbol in scopes and
    /// the global symbol table (function symbols).
    fn get_global_type_bound(&self, symbol: &mut Symbol) -> Option<TypeBound> {
        match &symbol {
            Symbol::Variable { .. } | Symbol::Expression { .. } => {
                self.get_local_type_bound(symbol)
            }
            Symbol::Parameter { function, name, .. } => {
                let params = self.func_params.get(function).unwrap();
                params.get(name).cloned()
            }
            Symbol::Return { function, .. } => self.func_returns.get(function).cloned(),
        }
    }

    /// Update the corresponding type bound of the given symbol in local scopes.
    fn update_local_type_bound(&mut self, symbol: Symbol, type_bound: TypeBound) {
        let scope = &mut self.scopes[symbol.get_scope()];
        scope.symbols.insert(symbol, type_bound);
    }

    /// Update the corresponding type bound of the given symbol in top scopes.
    fn update_global_type_bound(&mut self, symbol: Symbol, type_bound: TypeBound) {
        match &symbol {
            Symbol::Variable { .. } | Symbol::Expression { .. } => {
                let scope = &mut self.scopes[symbol.get_scope()];
                scope.symbols.insert(symbol, type_bound);
            }
            Symbol::Parameter { function, name, .. } => {
                let params = self.func_params.get_mut(function).unwrap();
                params.insert(name.clone(), type_bound);
            }
            Symbol::Return { function, .. } => {
                self.func_returns.insert(function.clone(), type_bound);
            }
        }
    }

    /// Get variable and expressions symbols and type bounds in all scopes,
    /// with function symbols from the global symbol table.
    fn get_global_symbols(&self) -> Vec<(Symbol, TypeBound)> {
        self.scopes
            .iter()
            .map(|scope| {
                scope
                    .symbols
                    .clone()
                    .into_iter()
                    .map(|(symbol, type_bound)| match &symbol {
                        Symbol::Variable { .. } | Symbol::Expression { .. } => (symbol, type_bound),
                        Symbol::Parameter { function, name, .. } => {
                            (symbol.clone(), self.func_params[function][name].clone())
                        }
                        Symbol::Return { function, .. } => {
                            (symbol.clone(), self.func_returns[function].clone())
                        }
                    })
            })
            .flatten()
            .collect()
    }

    /// Resolve all symbols to concrete types.
    fn resolve_symbols(&mut self, errors: &mut Vec<(String, Location)>) {
        // Merge add constraints of function symbols into one type bound
        // in the symbol table.
        self.merge_function_symbols();

        // Make all functions do not have `return` or return nothing
        // have void return type.
        self.implicit_return();

        // Perform the first pass of resolving symbols.
        let mut modified = true;
        while modified {
            modified = false;
            for (symbol, type_bound) in self.get_global_symbols() {
                modified |= self.resolve_symbol(symbol, type_bound, errors)
            }
        }

        // Perform heuristic type inference for each symbol, and then
        // resolving all symbols until stable after each inference.
        while let Some(_) = self
            .get_global_symbols()
            .iter()
            .find(|(s, b)| self.heuristic_resolving(s.clone(), b.clone(), errors))
        {
            let mut modified = true;
            while modified {
                modified = false;
                for (symbol, type_bound) in self.get_global_symbols() {
                    modified |= self.resolve_symbol(symbol, type_bound, errors)
                }
            }
        }
    }

    /// Merge add constraints of function symbols into one type bound
    /// in the symbol table.
    fn merge_function_symbols(&mut self) {
        let symbols = self
            .scopes
            .iter()
            .map(|scope| scope.symbols.iter())
            .flatten();
        for (symbol, type_bound) in symbols {
            match symbol {
                Symbol::Parameter { function, name, .. } => {
                    let old_type_bound = &self.func_params[function][name];
                    let type_bound = TypeBound::merge(old_type_bound, type_bound);
                    self.func_params
                        .get_mut(function)
                        .unwrap()
                        .insert(name.to_string(), type_bound);
                }
                Symbol::Return { function, .. } => {
                    let old_type_bound = &self.func_returns[function];
                    let type_bound = TypeBound::merge(old_type_bound, type_bound);
                    self.func_returns.insert(function.to_string(), type_bound);
                }
                _ => (),
            }
        }
    }

    /// Make all functions do not have `return` or return nothing
    /// have void return type.
    fn implicit_return(&mut self) {
        for type_bound in self.func_returns.values_mut() {
            if type_bound.upper.is_empty()
                && type_bound.lower.is_empty()
                && type_bound.bounded.is_none()
            {
                type_bound.bounded = Some(Type::Void(None));
            }
        }
    }

    /// Perform heuristic type inference for a symbol. Specifically,
    /// We will ignore all unknown symbol bounds.
    fn heuristic_resolving(
        &mut self,
        symbol: Symbol,
        mut type_bound: TypeBound,
        errors: &mut Vec<(String, Location)>,
    ) -> bool {
        type_bound.upper = type_bound
            .upper
            .into_iter()
            .filter(|bound| {
                if let Bound::Symbol { mut refer, .. } = bound.clone() {
                    let target = self.get_global_type_bound(&mut refer).unwrap();
                    target.bounded.is_some()
                } else {
                    true
                }
            })
            .collect();

        type_bound.lower = type_bound
            .lower
            .into_iter()
            .filter(|bound| {
                if let Bound::Symbol { mut refer, .. } = bound.clone() {
                    let target = self.get_global_type_bound(&mut refer).unwrap();
                    target.bounded.is_some()
                } else {
                    true
                }
            })
            .collect();

        self.resolve_symbol(symbol, type_bound, errors)
    }

    /// Try to resolve a symbol by checking whether all other symbols
    /// that bound it have been fully bounded. Return true if this
    /// symbol is successfully bounded this time.
    fn resolve_symbol(
        &mut self,
        symbol: Symbol,
        mut type_bound: TypeBound,
        errors: &mut Vec<(String, Location)>,
    ) -> bool {
        // Check for the bounded/wrapped type.
        let mut has_bounded = true;
        if type_bound.bounded.is_none() {
            if let Some(typ) = &type_bound.wrapped {
                if let Some(typ) = typ.borrow().specialized() {
                    type_bound.bounded = Some(typ);
                    self.update_global_type_bound(symbol.clone(), type_bound.clone());
                    has_bounded = false;
                }
            }
        }

        // Unify upper bounds.
        let mut upper = Type::Any;
        for bound in type_bound.upper.iter_mut() {
            match bound {
                Bound::Type {
                    refer,
                    pointer,
                    location,
                } => {
                    let refer = match self.pointer_transformation(refer, pointer) {
                        Some(refer) => refer,
                        None => {
                            let message = format!("`{}` should be a pointer.", symbol);
                            errors.push((message, Location::default()));
                            return false;
                        }
                    };
                    upper = match Type::compare_types(&upper, &refer) {
                        TypeRelationship::Sub => upper,
                        TypeRelationship::Equal => upper,
                        TypeRelationship::Super => refer.clone(),
                        TypeRelationship::Invalid => {
                            let message = format!(
                                "Incompatible types `{}` and `{}` for `{}`.",
                                upper, refer, symbol
                            );
                            errors.push((message, location.clone()));
                            return false;
                        }
                    };
                }
                Bound::Symbol {
                    refer,
                    pointer,
                    location,
                } => {
                    let type_bound = self.get_global_type_bound(refer).unwrap();
                    if let Some(bounded) = &type_bound.bounded {
                        let bounded = match self.pointer_transformation(bounded, pointer) {
                            Some(bounded) => bounded,
                            None => {
                                let message = format!("`{}` should be a pointer.", symbol);
                                errors.push((message, Location::default()));
                                return false;
                            }
                        };
                        upper = match Type::compare_types(&upper, &bounded) {
                            TypeRelationship::Sub => upper,
                            TypeRelationship::Equal => upper,
                            TypeRelationship::Super => bounded.clone(),
                            TypeRelationship::Invalid => {
                                let message = format!(
                                    "Incompatible types `{}` and `{}` for `{}`.",
                                    upper, bounded, symbol
                                );
                                errors.push((message, location.clone()));
                                return false;
                            }
                        }
                    } else {
                        return false;
                    }
                }
            }
        }

        // Unify lower bounds.
        let mut lower = Type::Nothing;
        for bound in type_bound.lower.iter_mut() {
            match bound {
                Bound::Type {
                    refer,
                    pointer,
                    location,
                } => {
                    let refer = match self.pointer_transformation(refer, pointer) {
                        Some(refer) => refer,
                        None => {
                            let message = format!("`{}` should be a pointer.", symbol);
                            errors.push((message, Location::default()));
                            return false;
                        }
                    };
                    lower = match Type::compare_types(&lower, &refer) {
                        TypeRelationship::Sub => refer.clone(),
                        TypeRelationship::Equal => lower,
                        TypeRelationship::Super => lower,
                        TypeRelationship::Invalid => {
                            let message = format!(
                                "Incompatible types `{}` and `{}` for `{}`.",
                                lower, refer, symbol
                            );
                            errors.push((message, location.clone()));
                            return false;
                        }
                    };
                }
                Bound::Symbol {
                    refer,
                    pointer,
                    location,
                } => {
                    let type_bound = self.get_global_type_bound(refer).unwrap();
                    if let Some(bounded) = &type_bound.bounded {
                        let bounded = match self.pointer_transformation(bounded, pointer) {
                            Some(bounded) => bounded,
                            None => {
                                let message = format!("`{}` should be a pointer.", symbol);
                                errors.push((message, Location::default()));
                                return false;
                            }
                        };
                        lower = match Type::compare_types(&lower, &bounded) {
                            TypeRelationship::Sub => bounded.clone(),
                            TypeRelationship::Equal => lower,
                            TypeRelationship::Super => lower,
                            TypeRelationship::Invalid => {
                                let message = format!(
                                    "Incompatible types `{}` and `{}` for `{}`.",
                                    lower, bounded, symbol
                                );
                                errors.push((message, location.clone()));
                                return false;
                            }
                        };
                    } else {
                        return false;
                    }
                }
            }
        }

        // Unify `infix`.
        let mut infix = Type::Nothing;
        if let Some((left_symbol, right_symbol)) = &mut type_bound.infix {
            let left_type_bound = self.get_global_type_bound(left_symbol).unwrap();
            let right_type_bound = self.get_global_type_bound(right_symbol).unwrap();
            if let (Some(left), Some(right)) = &(left_type_bound.bounded, right_type_bound.bounded)
            {
                infix = match Type::compare_types(left, right) {
                    TypeRelationship::Sub => right.clone(),
                    TypeRelationship::Equal => right.clone(),
                    TypeRelationship::Super => left.clone(),
                    TypeRelationship::Invalid => {
                        let message = format!(
                            "Incompatible types `{}` and `{}` for `{}`.",
                            left, right, symbol
                        );
                        errors.push((message, left.locate()));
                        return false;
                    }
                }
            } else {
                return false;
            }
        }

        // Unify `upper`, `lower`, and `infix`.
        let mut type_ = match Type::compare_types(&infix, &lower) {
            TypeRelationship::Sub => lower,
            TypeRelationship::Equal => lower,
            TypeRelationship::Super => infix,
            TypeRelationship::Invalid => {
                let message = format!(
                    "Incompatible types `{}` and `{}` for `{}`.",
                    infix, lower, symbol
                );
                errors.push((message, Location::default()));
                return false;
            }
        };
        type_ = match Type::compare_types(&upper, &type_) {
            TypeRelationship::Sub => {
                let message = format!(
                    "`{}` should be more powerful than `{}` for `{}`.",
                    upper, type_, symbol
                );
                errors.push((message, Location::default()));
                return false;
            }
            TypeRelationship::Equal => type_,
            TypeRelationship::Super => type_,
            TypeRelationship::Invalid => {
                let message = format!(
                    "Incompatible types `{}` and `{}` for `{}`.",
                    upper, type_, symbol
                );
                errors.push((message, Location::default()));
                return false;
            }
        };

        // Perform `member` transformation.
        if let Some(member) = &type_bound.member {
            if let Type::Struct { members, .. } = type_ {
                type_ = match members.get(member) {
                    Some(typ) => typ.clone(),
                    None => {
                        let message = format!(
                            "`{}` is not a valid member for structure `{}`.",
                            member, symbol
                        );
                        errors.push((message, Location::default()));
                        return false;
                    }
                }
            } else {
                let message = format!("`{}` should be a structure.", symbol);
                errors.push((message, Location::default()));
                return false;
            }
        }

        // Perform `ptr_mem` transformation.
        if let Some(member) = &type_bound.ptr_mem {
            if let Type::Pointer { refer, .. } = type_ {
                if let Type::Struct { members, .. } = *refer {
                    type_ = match members.get(member) {
                        Some(typ) => typ.clone(),
                        None => {
                            let message = format!(
                                "`{}` is not a valid member for structure `{}`.",
                                member, symbol
                            );
                            errors.push((message, Location::default()));
                            return false;
                        }
                    }
                } else {
                    let message = format!("`{}` should be a pointer to a structure.", symbol);
                    errors.push((message, Location::default()));
                    return false;
                }
            } else {
                let message = format!("`{}` should be a pointer to a structure.", symbol);
                errors.push((message, Location::default()));
                return false;
            }
        }

        // Unify `type_` and `type_bound.bounded`.
        if let Some(bounded) = &type_bound.bounded {
            match Type::compare_types(&type_, bounded) {
                TypeRelationship::Sub => (),
                TypeRelationship::Equal => (),
                TypeRelationship::Super => {
                    let message = format!(
                        "`{}` should be less powerful than `{}` for `{}`.",
                        type_, bounded, symbol
                    );
                    errors.push((message, Location::default()));
                }
                TypeRelationship::Invalid => {
                    let message = format!(
                        "Incompatible types `{}` and `{}` for `{}`.",
                        type_, bounded, symbol
                    );
                    errors.push((message, bounded.locate()));
                }
            }
            if has_bounded {
                false
            } else {
                true
            }
        } else {
            // Promote `Type::Byte` to `Type::Short`, and reject other dummy types.
            match type_ {
                Type::Byte => {
                    type_bound.bounded = Some(Type::Short(None));
                    self.update_global_type_bound(symbol.clone(), type_bound.clone());
                    true
                }
                typ if Type::is_dummy_type(&typ) => false,
                typ => {
                    type_bound.bounded = Some(typ);
                    self.update_global_type_bound(symbol.clone(), type_bound.clone());
                    true
                }
            }
        }
    }

    /// Perform `pointer` transformation.
    fn pointer_transformation(&self, type_: &Type, pointer: &Option<bool>) -> Option<Type> {
        match pointer {
            Some(true) => Some(Type::Pointer {
                refer: Box::new(type_.clone()),
                location: None,
            }),
            Some(false) => {
                if let Type::Pointer { refer, .. } = type_ {
                    Some((**refer).clone())
                } else {
                    None
                }
            }
            None => Some(type_.clone()),
        }
    }

    /// Update bounded types back to AST.
    fn update_wrapped(&self, errors: &mut Vec<(String, Location)>) {
        // Update function return types.
        for (func, return_type_bound) in self.func_returns.iter() {
            if let Some(type_) = &return_type_bound.bounded {
                return_type_bound
                    .wrapped
                    .as_ref()
                    .unwrap()
                    .borrow_mut()
                    .specialize(type_);
            } else {
                let message = format!("Unresolved function return type `{}`.", func);
                errors.push((message, Location::default()));
            }
        }

        // Update function parameters.
        for (func, params) in self.func_params.iter() {
            for (param, type_bound) in params {
                if let Some(type_) = &type_bound.bounded {
                    type_bound
                        .wrapped
                        .as_ref()
                        .unwrap()
                        .borrow_mut()
                        .specialize(type_);
                } else {
                    let message =
                        format!("Unresolved function parameter `{}` in `{}`.", param, func);
                    errors.push((message, Location::default()));
                }
            }
        }

        // Update local variables.
        let symbols = self
            .scopes
            .iter()
            .map(|scope| scope.symbols.iter())
            .flatten();
        for (symbol, type_bound) in symbols {
            if let Symbol::Variable { .. } = symbol {
                if let Some(type_) = &type_bound.bounded {
                    type_bound
                        .wrapped
                        .as_ref()
                        .unwrap()
                        .borrow_mut()
                        .specialize(type_);
                } else {
                    let message = format!("Unresolved symbol `{}`.", symbol);
                    errors.push((message, Location::default()));
                }
            } else if let Symbol::Expression { .. } = symbol {
                if type_bound.bounded.is_none() {
                    let message = format!("Unresolved symbol `{}`.", symbol);
                    errors.push((message, Location::default()));
                }
            }
        }
    }
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
        let mut errors = Vec::new();
        self.symbol_table.resolve_symbols(&mut errors);
        self.symbol_table.update_wrapped(&mut errors);
        for (msg, loc) in errors {
            self.push_error(&msg, &loc);
        }

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
            self.symbol_table.define_symbol(&symbol, &type_bound);
        }

        // Define the function return.
        let symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: name.clone(),
        };
        let type_bound = TypeBound::wrapped(return_type);
        self.symbol_table.define_symbol(&symbol, &type_bound);

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
            Statement::Return {
                expression,
                location,
            } => self.analyse_statement_return(expression, location),
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
        let mut return_symbol = Symbol::Return {
            scope: self.symbol_table.current_scope,
            function: current_func.clone(),
        };
        let mut return_type_bound = self
            .symbol_table
            .get_local_type_bound(&mut return_symbol)
            .unwrap();

        // Add constraints.
        match expression {
            Some(expr) => {
                if let Ok((expr_symbol, mut expr_type_bound)) = self.analyse_expression(expr) {
                    return_type_bound.lower.push(Bound::Symbol {
                        refer: expr_symbol.clone(),
                        pointer: None,
                        location: expr.locate(),
                    });
                    expr_type_bound.upper.push(Bound::Symbol {
                        refer: return_symbol.clone(),
                        pointer: None,
                        location: expr.locate(),
                    });
                    self.symbol_table
                        .update_local_type_bound(return_symbol, return_type_bound);
                    self.symbol_table
                        .update_local_type_bound(expr_symbol, expr_type_bound);
                }
            }
            None => {
                let type_void = Type::Void(None);
                return_type_bound.upper.push(Bound::Type {
                    refer: type_void.clone(),
                    pointer: None,
                    location: location.clone(),
                });
                return_type_bound.lower.push(Bound::Type {
                    refer: type_void.clone(),
                    pointer: None,
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
        if let Type::Struct {
            name,
            members,
            location,
        } = base_type.borrow().clone()
        {
            // Define the structure.
            self.symbol_table.define_structure(&Type::Struct {
                name,
                members: members.clone(),
                location,
            });

            for (type_, name, init) in declarators {
                // Define the variable.
                let symbol = Symbol::Variable {
                    scope: self.symbol_table.current_scope,
                    name: name.clone(),
                    location: type_.borrow().locate(),
                };
                let mut type_bound = TypeBound::wrapped(&type_);
                type_bound.upper.push(Bound::Type {
                    refer: type_.borrow().clone(),
                    pointer: None,
                    location: type_.borrow().locate(),
                });
                type_bound.lower.push(Bound::Type {
                    refer: type_.borrow().clone(),
                    pointer: None,
                    location: type_.borrow().locate(),
                });

                // Check the initializer.
                match init {
                    Some(Expression::InitList {
                        initializers,
                        location,
                    }) => {
                        let initializers: Vec<_> = initializers
                            .iter()
                            .map(|(name, expr)| (name, self.analyse_expression(expr)))
                            .collect();
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
                                Some(type_) => expr_type_bound.upper.push(Bound::Type {
                                    refer: type_.clone(),
                                    pointer: None,
                                    location: location.clone(),
                                }),
                                None => {
                                    let message = format!("Unknown field `{}` in InitList.", field);
                                    self.push_error(&message, location);
                                    return;
                                }
                            }

                            // Update the type bound.
                            self.symbol_table
                                .update_local_type_bound(expr_symbol, expr_type_bound);
                        }
                    }
                    Some(_) => {
                        let message = "Expect an init list.";
                        self.push_error(&message, &type_.borrow().locate());
                        return;
                    }
                    None => (),
                }

                // Update the type bound.
                self.symbol_table
                    .update_local_type_bound(symbol, type_bound);
            }
        } else {
            // Find other variables.
            for (type_, name, init) in declarators {
                // Define the variable.
                let symbol = Symbol::Variable {
                    scope: self.symbol_table.current_scope,
                    name: name.clone(),
                    location: type_.borrow().locate(),
                };
                let mut type_bound = TypeBound::wrapped(type_);
                type_bound.upper.push(Bound::Type {
                    refer: type_.borrow().clone(),
                    pointer: None,
                    location: type_.borrow().locate(),
                });
                type_bound.lower.push(Bound::Type {
                    refer: type_.borrow().clone(),
                    pointer: None,
                    location: type_.borrow().locate(),
                });

                // Check the initializer.
                match init {
                    Some(Expression::InitList {
                        initializers,
                        location,
                    }) => {
                        // Find an array initializer.
                        let initializers: Vec<_> = initializers
                            .iter()
                            .map(|(name, expr)| (name, self.analyse_expression(expr)))
                            .collect();
                        for (field, expr) in initializers {
                            // An array initializer should not have named fields.
                            if field.is_some() {
                                let message = "Named fields in InitList.";
                                self.push_error(&message, location);
                                return;
                            }

                            // Change initializers to the array type.
                            if expr.is_err() {
                                return;
                            }
                            let (expr_symbol, mut expr_type_bound) = expr.unwrap();
                            if let Some(bounded) = expr_type_bound.bounded {
                                expr_type_bound.bounded = Some(Type::Array {
                                    content: Box::new(bounded),
                                    length: None,
                                    location: None,
                                });
                            } else {
                                let message = "Array initializers should be constants";
                                self.push_error(&message, location);
                                return;
                            }

                            // Add constraints.
                            type_bound.lower.push(Bound::Symbol {
                                refer: expr_symbol.clone(),
                                pointer: None,
                                location: location.clone(),
                            });
                            expr_type_bound.upper.push(Bound::Symbol {
                                refer: symbol.clone(),
                                pointer: None,
                                location: location.clone(),
                            });

                            // Update the type bound.
                            self.symbol_table
                                .update_local_type_bound(expr_symbol, expr_type_bound);
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
                        type_bound.lower.push(Bound::Symbol {
                            refer: expr_symbol.clone(),
                            pointer: None,
                            location: location.clone(),
                        });
                        expr_type_bound.upper.push(Bound::Symbol {
                            refer: symbol.clone(),
                            pointer: None,
                            location: location.clone(),
                        });

                        // Update the type bound.
                        self.symbol_table
                            .update_local_type_bound(expr_symbol, expr_type_bound);
                    }
                    None => (),
                }

                // Update the type bound.
                self.symbol_table
                    .update_local_type_bound(symbol, type_bound);
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
            Expression::CharConst { location, .. } => self.analyse_expression_charconst(location),
            Expression::StrConst { location, .. } => self.analyse_expression_strconst(location),
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
        let mut symbol = if self.symbol_table.is_parameter(value) {
            Symbol::Parameter {
                function: self.symbol_table.current_func.clone().unwrap(),
                scope: self.symbol_table.current_scope,
                name: value.to_string(),
            }
        } else {
            Symbol::Variable {
                scope: self.symbol_table.current_scope,
                name: value.to_string(),
                location: location.clone(),
            }
        };
        self.symbol_table
            .get_local_type_bound(&mut symbol)
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
        let symbol = self.symbol_table.make_expression_symbol(location);
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table.define_symbol(&symbol, &type_bound);
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
        let symbol = self.symbol_table.make_expression_symbol(location);
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table.define_symbol(&symbol, &type_bound);
        Ok((symbol, type_bound))
    }

    fn analyse_expression_charconst(
        &mut self,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Char(None);
        let symbol = self.symbol_table.make_expression_symbol(location);
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table.define_symbol(&symbol, &type_bound);
        Ok((symbol, type_bound))
    }

    fn analyse_expression_strconst(
        &mut self,
        location: &Location,
    ) -> Result<(Symbol, TypeBound), ()> {
        let type_ = Type::Pointer {
            refer: Box::new(Type::Char(None)),
            location: None,
        };
        let symbol = self.symbol_table.make_expression_symbol(location);
        let type_bound = TypeBound::bounded(&type_);
        self.symbol_table.define_symbol(&symbol, &type_bound);
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
                expr_type_bound.upper.push(Bound::Type {
                    refer: Type::Double(None),
                    pointer: None,
                    location: location.clone(),
                });
                expr_type_bound.lower.push(Bound::Type {
                    refer: Type::Byte,
                    pointer: None,
                    location: location.clone(),
                });
                let symbol = self.symbol_table.make_expression_symbol(location);
                let type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: expr_symbol.clone(),
                    pointer: None,
                    location: location.clone(),
                });
                self.symbol_table.define_symbol(&symbol, &type_bound);
                self.symbol_table
                    .update_local_type_bound(expr_symbol, expr_type_bound);
                Ok((symbol, type_bound))
            }
            "&" => {
                // Allow any types.
                // Set `TypeBound::pointer`.
                let symbol = self.symbol_table.make_expression_symbol(location);
                let type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: expr_symbol.clone(),
                    pointer: Some(true),
                    location: location.clone(),
                });
                self.symbol_table.define_symbol(&symbol, &type_bound);
                Ok((symbol, type_bound))
            }
            "*" => {
                // Allow pointer types.
                // Set `TypeBound::pointer`.
                let symbol = self.symbol_table.make_expression_symbol(location);
                let type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: expr_symbol.clone(),
                    pointer: Some(false),
                    location: location.clone(),
                });
                self.symbol_table.define_symbol(&symbol, &type_bound);
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
        let (left_symbol, mut left_type_bound) = self.analyse_expression(left)?;
        let symbol = self.symbol_table.make_expression_symbol(&left.locate());
        let type_bound = match operator {
            "+" | "-" | "*" | "/" | "%" => {
                // Allow any types.
                // Set `TypeBound::infix`.
                let (right_symbol, _) = self.analyse_expression(right)?;
                TypeBound::infixed(&left_symbol, &right_symbol)
            }
            "=" | "+=" | "-=" | "*=" | "/=" | "%=" => {
                // Allow any types.
                // return the left type bound whose lower is bounded by the right.
                let (right_symbol, _) = self.analyse_expression(right)?;
                left_type_bound.lower.push(Bound::Symbol {
                    refer: right_symbol,
                    pointer: None,
                    location: right.locate(),
                });
                self.symbol_table
                    .update_local_type_bound(left_symbol.clone(), left_type_bound.clone());
                left_type_bound
            }
            "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||" => {
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
                let mut type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: left_symbol.clone(),
                    pointer: None,
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
                let mut type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: left_symbol.clone(),
                    pointer: None,
                    location: left.locate(),
                });
                type_bound.ptr_mem = Some(name.to_string());
                type_bound
            }
            _ => unreachable!(),
        };

        self.symbol_table.define_symbol(&symbol, &type_bound);
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
                let symbol = self
                    .symbol_table
                    .make_expression_symbol(&expression.locate());
                let type_bound = TypeBound::squeezed(&Bound::Symbol {
                    refer: expr_symbol.clone(),
                    pointer: None,
                    location: expression.locate(),
                });
                self.symbol_table.define_symbol(&symbol, &type_bound);
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
        let (expr_symbol, mut expr_type_bound) = self.analyse_expression(expression)?;
        let (index_symbol, mut index_type_bound) = self.analyse_expression(index)?;

        // index must be integral types.
        index_type_bound.upper.push(Bound::Type {
            refer: Type::Long(None),
            pointer: None,
            location: index.locate(),
        });
        index_type_bound.lower.push(Bound::Type {
            refer: Type::Byte,
            pointer: None,
            location: index.locate(),
        });
        self.symbol_table
            .update_local_type_bound(index_symbol.clone(), index_type_bound.clone());

        // Require the expression to be a pointer.
        expr_type_bound.upper.push(Bound::Type {
            refer: Type::AnyRef,
            pointer: None,
            location: expression.locate(),
        });
        expr_type_bound.lower.push(Bound::Type {
            refer: Type::Null,
            pointer: None,
            location: expression.locate(),
        });
        let symbol = self
            .symbol_table
            .make_expression_symbol(&expression.locate());
        let type_bound = TypeBound::squeezed(&Bound::Symbol {
            refer: expr_symbol.clone(),
            pointer: Some(true),
            location: expression.locate(),
        });
        self.symbol_table.define_symbol(&symbol, &type_bound);
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
                .define_symbol(&return_symbol, &return_type_bound);

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
            for (arg, (param, mut param_type_bound)) in arguments.iter().zip(params.into_iter()) {
                let (arg_symbol, mut arg_type_bound) = self.analyse_expression(arg)?;
                let param_symbol = Symbol::Parameter {
                    scope: self.symbol_table.current_scope,
                    function: value.to_string(),
                    name: param.to_string(),
                };
                arg_type_bound.upper.push(Bound::Symbol {
                    refer: param_symbol.clone(),
                    pointer: None,
                    location: arg.locate(),
                });
                param_type_bound.lower.push(Bound::Symbol {
                    refer: arg_symbol.clone(),
                    pointer: None,
                    location: arg.locate(),
                });
                self.symbol_table
                    .define_symbol(&arg_symbol, &arg_type_bound);
                self.symbol_table
                    .define_symbol(&param_symbol, &param_type_bound);
            }

            // Set up the function call symbol.
            let symbol = self
                .symbol_table
                .make_expression_symbol(&expression.locate());
            let mut type_bound = TypeBound::new();
            type_bound.lower.push(Bound::Symbol {
                refer: return_symbol.clone(),
                pointer: None,
                location: expression.locate(),
            });
            self.symbol_table
                .define_symbol(&symbol.clone(), &type_bound.clone());
            Ok((symbol, type_bound))
        } else {
            // Require the function name directly available.
            let message = "Require a function name.";
            self.push_error(&message, &expression.locate());
            return Err(());
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    // use crate::lexer::Lexer;
    // use crate::parser::Parser;
    // use crate::preprocessor::Preprocessor;

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
}
