use crate::structure::{Error, StaticObject};

pub struct Resolver<'a> {
    generic_ast: Option<Vec<StaticObject>>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Resolver<'a> {
    pub fn new(generic_ast: Vec<StaticObject>, errors: &'a mut Vec<Error>) -> Resolver<'a> {
        Resolver {
            generic_ast: Some(generic_ast),
            errors,
        }
    }

    pub fn run(&mut self) -> Result<Vec<StaticObject>, ()> {
        Ok(self.generic_ast.take().unwrap())
    }
}

// //! A resolver resolving all dummy types in a vector of functions.

// use std::collections::HashMap;

// use indexmap::IndexMap;

// use crate::structure::{
//     Array, Error, Expression, Function, Locate, Location, Pointer, Statement, Type,
// };

// /// A structure containg names defined in each scopes and their type information.
// struct SymbolTable<'a> {
//     functions: HashMap<&'a str, (Type, IndexMap<&'a str, Type>)>, // value: (return type, parameters)
//     tables: Vec<HashMap<&'a str, Type>>,
//     current_func: Option<&'a str>,
// }

// impl<'a> SymbolTable<'a> {
//     fn new() -> SymbolTable<'a> {
//         SymbolTable {
//             functions: HashMap::new(),
//             tables: vec![HashMap::new()],
//             current_func: None,
//         }
//     }

//     fn enter(&mut self) {
//         self.tables.push(HashMap::new());
//     }

//     fn leave(&mut self) {
//         self.tables.pop();
//     }

//     /// Define names in the current scope.
//     fn insert(&mut self, name: &'a str, r#type: Type) {
//         self.tables.last_mut().unwrap().insert(name, r#type);
//     }

//     fn get(&self, name: &str) -> Option<Type> {
//         self.tables
//             .iter()
//             .rev()
//             .find_map(|env| env.get(name))
//             .cloned()
//     }

//     /// Get the return type of the function that is currently being resolved.
//     fn get_return_type(&self) -> Type {
//         let name = self.current_func.unwrap();
//         self.functions.get(name).unwrap().0
//     }

//     /// Update the return type of the function that is currently being resolved.
//     fn update_return_type(&mut self, return_type: Type) {
//         let name = self.current_func.unwrap();
//         let (_, parameters) = self.functions.remove(name).unwrap();
//         self.functions.insert(name, (return_type, parameters));
//     }

//     /// Update parameters of the function that is currently being resolved.
//     fn update_parameters(&mut self, parameters: IndexMap<&'a str, Type>) {
//         let name = self.current_func.unwrap();
//         let (return_type, _) = self.functions.remove(name).unwrap();
//         self.functions.insert(name, (return_type, parameters));
//     }

//     fn update_generic_ident(&mut self, generic_ident: Option<&'a str>, r#type: Type) {
//         if let Some(name) = generic_ident {
//             self.tables
//                 .iter_mut()
//                 .rev()
//                 .find(|env| env.get(name).is_some())
//                 .unwrap()
//                 .insert(name, r#type);
//         }
//     }
// }

// /// A resolver resolving all dummy types in a vector of functions.
// ///
// /// Read the project README for how it works.
// pub struct Resolver<'a> {
//     generic_ast: Option<Vec<Function<'a>>>,
//     errors: Option<Vec<Error>>,
//     symbol_table: SymbolTable<'a>,
// }

// impl<'a> Resolver<'a> {
//     pub fn new(generic_ast: Vec<Function<'a>>, errors: Vec<Error>) -> Resolver {
//         Resolver {
//             generic_ast: Some(generic_ast),
//             errors: Some(errors),
//             symbol_table: SymbolTable::new(),
//         }
//     }

//     pub fn run(&mut self) -> (Vec<Function<'a>>, Vec<Error>) {
//         // Load all functions' names into the symbol table before actual inference.
//         // Fail and return if there are multiple functions with the same name.
//         let mut multi_func_flag = false;
//         let ast: Vec<_> = self
//             .generic_ast
//             .take()
//             .unwrap()
//             .into_iter()
//             .map(|func| {
//                 let name = func.name;
//                 let return_type = func.r#type;
//                 let parameters = func.parameters.clone();
//                 if self.symbol_table.functions.contains_key(name) {
//                     self.push_error("Multiple function definitions.", func.locate());
//                     multi_func_flag = true;
//                 } else {
//                     self.symbol_table
//                         .functions
//                         .insert(name, (return_type, parameters));
//                 }
//                 func
//             })
//             .collect();
//         if multi_func_flag {
//             return (ast, self.errors.take().unwrap());
//         }
//         // Perform type inference for every function.
//         let ast: Vec<_> = ast
//             .into_iter()
//             .map(|func| self.resolve_function(func))
//             .collect();
//         // Merge type information of functions in the symbol table back to AST.
//         let ast: Vec<_> = ast
//             .into_iter()
//             .map(|mut func| {
//                 let name = func.name;
//                 let location = func.locate();
//                 let (return_type, parameters) = self.symbol_table.functions.remove(name).unwrap();
//                 match return_type {
//                     Type::T { .. } => {
//                         let message = format!("Unresolved return type of function `{}`.", name);
//                         self.push_error(&message, location);
//                     }
//                     _ => func.r#type = return_type,
//                 }
//                 for (param, r#type) in &parameters {
//                     if let Type::T { .. } = r#type {
//                         let message =
//                             format!("Unresolved parameter `{}` in function `{}`.", param, name);
//                         self.push_error(&message, location);
//                     }
//                 }
//                 func.parameters = parameters;
//                 func
//             })
//             .collect();
//         (ast, self.errors.take().unwrap())
//     }

//     fn push_error(&mut self, message: &str, location: Location) {
//         self.errors.as_mut().unwrap().push(Error::Resolving {
//             message: message.to_string(),
//             location,
//         });
//     }

//     /// Conceptually, we are trying to find type constraints that allow an assignment from
//     /// a value with `type_right` to a variable with `type_left`.
//     fn unify_types(&self, type_left: Type, type_right: Type) -> Result<(Type, Type), ()> {
//         use Type::*;
//         // `T` and `void` types cannot be arrays or pointers.
//         if let T { .. } = type_left {
//             return Ok((type_right, type_right));
//         }
//         if let T { .. } = type_right {
//             return Ok((type_left, type_left));
//         }
//         if let (Void { .. }, Void { .. }) = (type_left, type_right) {
//             return Ok((type_left, type_right));
//         }
//         // For other types, they must both or neither be arrays/pointers.
//         if type_left.get_array() != type_right.get_array()
//             || type_left.get_pointer_flag() != type_right.get_pointer_flag()
//         {
//             return Err(());
//         }
//         // Allow safe type coercion.
//         match type_left {
//             Char { .. } => match type_right {
//                 Char { .. } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Short {
//                 signed_flag: true, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short {
//                     signed_flag: true, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Short {
//                 signed_flag: false, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short {
//                     signed_flag: false, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Int {
//                 signed_flag: true, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short { .. }
//                 | Int {
//                     signed_flag: true, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Int {
//                 signed_flag: false, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short {
//                     signed_flag: false, ..
//                 }
//                 | Int {
//                     signed_flag: false, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Long {
//                 signed_flag: true, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short { .. }
//                 | Int { .. }
//                 | Long {
//                     signed_flag: true, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Long {
//                 signed_flag: false, ..
//             } => match type_right {
//                 Char { .. }
//                 | Short {
//                     signed_flag: false, ..
//                 }
//                 | Int {
//                     signed_flag: false, ..
//                 }
//                 | Long {
//                     signed_flag: false, ..
//                 } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             Float { .. } => match type_right {
//                 Char { .. } | Short { .. } | Int { .. } | Long { .. } | Float { .. } => {
//                     Ok((type_left, type_right))
//                 }
//                 _ => Err(()),
//             },
//             Double { .. } => match type_right {
//                 Char { .. }
//                 | Short { .. }
//                 | Int { .. }
//                 | Long { .. }
//                 | Float { .. }
//                 | Double { .. } => Ok((type_left, type_right)),
//                 _ => Err(()),
//             },
//             _ => unreachable!(),
//         }
//     }

//     fn resolve_function(&mut self, function: Function<'a>) -> Function<'a> {
//         let Function {
//             r#type,
//             name,
//             parameters,
//             body,
//             location,
//         } = function;
//         // Set the environment.
//         self.symbol_table.current_func = Some(name);
//         self.symbol_table.enter();
//         // Load parameters as local variables.
//         for (param, r#type) in &parameters {
//             self.symbol_table.insert(param, *r#type);
//         }
//         // Resolve the body.
//         let body = self.resolve_statement(body);
//         // Merge type information of parameters back to functions.
//         let mut new_parameters = IndexMap::new();
//         for param in parameters.keys() {
//             let r#type = self.symbol_table.get(param).unwrap();
//             new_parameters.insert(*param, r#type);
//         }
//         self.symbol_table.update_parameters(new_parameters.clone());
//         // Clear the environment.
//         self.symbol_table.leave();
//         self.symbol_table.current_func = None;
//         Function {
//             r#type,
//             name,
//             parameters: new_parameters,
//             body,
//             location,
//         }
//     }

//     fn resolve_statement(&mut self, statement: Statement<'a>) -> Statement<'a> {
//         match statement {
//             stmt @ Statement::Continue(_) => stmt,
//             stmt @ Statement::Break(_) => stmt,
//             Statement::Expr(expr) => {
//                 let (_, expr, _) = self.resolve_expression(expr);
//                 Statement::Expr(expr)
//             }
//             Statement::Return {
//                 expression,
//                 location,
//             } => self.resolve_statement_return(expression, location),
//             Statement::Block {
//                 statements,
//                 location,
//             } => {
//                 self.symbol_table.enter();
//                 let statements: Vec<_> = statements
//                     .into_iter()
//                     .map(|stmt| self.resolve_statement(stmt))
//                     .collect();
//                 self.symbol_table.leave();
//                 Statement::Block {
//                     statements,
//                     location,
//                 }
//             }
//             Statement::Def {
//                 declarators,
//                 location,
//             } => self.resolve_statement_def(declarators, location),
//             Statement::While {
//                 condition,
//                 body,
//                 location,
//             } => {
//                 let (condition, body) =
//                     self.resolve_statement_while_base(condition, *body, location);
//                 Statement::While {
//                     condition,
//                     body: Box::new(body),
//                     location,
//                 }
//             }
//             Statement::Do {
//                 condition,
//                 body,
//                 location,
//             } => {
//                 let (condition, body) =
//                     self.resolve_statement_while_base(condition, *body, location);
//                 Statement::Do {
//                     condition,
//                     body: Box::new(body),
//                     location,
//                 }
//             }
//             Statement::For {
//                 initialization,
//                 condition,
//                 increment,
//                 body,
//                 location,
//             } => self.resolve_statement_for(initialization, condition, increment, *body, location),
//             Statement::If {
//                 condition,
//                 body,
//                 alternative,
//                 location,
//             } => self.resolve_statement_if(condition, *body, alternative, location),
//             Statement::Switch {
//                 expression,
//                 branches,
//                 default,
//                 location,
//             } => self.resolve_statement_switch(expression, branches, default, location),
//         }
//     }

//     fn resolve_statement_return(
//         &mut self,
//         expression: Option<Expression<'a>>,
//         location: Location,
//     ) -> Statement<'a> {
//         let (type_right, expression, generic_ident) = match expression {
//             Some(expr) => {
//                 let (r#type, expr, generic_ident) = self.resolve_expression(expr);
//                 (r#type, Some(expr), generic_ident)
//             }
//             None => (
//                 Type::Void {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//                 None,
//                 None,
//             ),
//         };
//         let type_left = self.symbol_table.get_return_type();
//         match self.unify_types(type_left, type_right) {
//             Ok((return_type, type_right)) => {
//                 self.symbol_table.update_return_type(return_type);
//                 self.symbol_table
//                     .update_generic_ident(generic_ident, type_right);
//             }
//             Err(_) => {
//                 let message = format!(
//                     "`{}` is expected to return `{}`, get `{}`.",
//                     self.symbol_table.current_func.unwrap(),
//                     type_left,
//                     type_right
//                 );
//                 self.push_error(&message, location);
//             }
//         }
//         Statement::Return {
//             expression,
//             location,
//         }
//     }

//     fn resolve_statement_def(
//         &mut self,
//         declarators: Vec<(Type, &'a str, Option<Expression<'a>>)>,
//         location: Location,
//     ) -> Statement<'a> {
//         let declarators: Vec<_> = declarators
//             .into_iter()
//             .map(|(r#type, ident, init)| {
//                 let (typ, expr, generic_ident) = match init {
//                     Some(expr) => {
//                         let (t, e, gi) = self.resolve_expression(expr);
//                         (t, Some(e), gi)
//                     }
//                     None => (
//                         Type::T {
//                             array_flag: false,
//                             array_len: None,
//                             pointer_flag: false,
//                             location: Location::empty(),
//                         },
//                         None,
//                         None,
//                     ),
//                 };
//                 let r#type = match self.unify_types(r#type, typ) {
//                     Ok((type_left, type_right)) => {
//                         self.symbol_table
//                             .update_generic_ident(generic_ident, type_right);
//                         type_left
//                     }
//                     Err(_) => {
//                         let message =
//                             format!("Expect `{}` to be `{}`, get `{}`.", ident, r#type, typ);
//                         self.push_error(&message, location);
//                         r#type
//                     }
//                 };
//                 self.symbol_table.insert(ident, r#type);
//                 (r#type, ident, expr)
//             })
//             .collect();
//         Statement::Def {
//             declarators,
//             location,
//         }
//     }

//     /// Parse function for both `while` and `do while`.
//     fn resolve_statement_while_base(
//         &mut self,
//         condition: Expression<'a>,
//         body: Statement<'a>,
//         location: Location,
//     ) -> (Expression<'a>, Statement<'a>) {
//         let dummy_int_signed = Type::make_dummy("Int", true, false, false);
//         let (r#type, condition, generic_ident) = self.resolve_expression(condition);
//         self.symbol_table
//             .update_generic_ident(generic_ident, dummy_int_signed);
//         if self.unify_types(dummy_int_signed, r#type).is_err() {
//             let message = format!(
//                 "Expect `{}` as the loop condition, get `{}`.",
//                 dummy_int_signed, r#type
//             );
//             self.push_error(&message, location);
//         }
//         let body = self.resolve_statement(body);
//         (condition, body)
//     }

//     fn resolve_statement_for(
//         &mut self,
//         initialization: Option<Expression<'a>>,
//         condition: Option<Expression<'a>>,
//         increment: Option<Expression<'a>>,
//         body: Statement<'a>,
//         location: Location,
//     ) -> Statement<'a> {
//         let initialization = match initialization {
//             Some(expr) => Some(self.resolve_expression(expr).1),
//             None => None,
//         };
//         let condition = match condition {
//             Some(expr) => Some(self.resolve_expression(expr).1),
//             None => None,
//         };
//         let increment = match increment {
//             Some(expr) => Some(self.resolve_expression(expr).1),
//             None => None,
//         };
//         let body = Box::new(self.resolve_statement(body));
//         Statement::For {
//             initialization,
//             condition,
//             increment,
//             body,
//             location,
//         }
//     }

//     fn resolve_statement_if(
//         &mut self,
//         condition: Expression<'a>,
//         body: Statement<'a>,
//         alternative: Option<Box<Statement<'a>>>,
//         location: Location,
//     ) -> Statement<'a> {
//         let dummy_int_signed = Type::make_dummy("Int", true, false, false);
//         let (r#type, condition, generic_ident) = self.resolve_expression(condition);
//         self.symbol_table
//             .update_generic_ident(generic_ident, dummy_int_signed);
//         if self.unify_types(dummy_int_signed, r#type).is_err() {
//             let message = format!("Expect `{}`, get `{}`.", dummy_int_signed, r#type);
//             self.push_error(&message, location);
//         }
//         let body = Box::new(self.resolve_statement(body));
//         let alternative = match alternative {
//             Some(stmt) => Some(Box::new(self.resolve_statement(*stmt))),
//             None => None,
//         };
//         Statement::If {
//             condition,
//             body,
//             alternative,
//             location,
//         }
//     }

//     fn resolve_statement_switch(
//         &mut self,
//         expression: Expression<'a>,
//         branches: Vec<(Expression<'a>, Vec<Statement<'a>>)>,
//         default: Option<Vec<Statement<'a>>>,
//         location: Location,
//     ) -> Statement<'a> {
//         let (_, expression, _) = self.resolve_expression(expression);
//         let branches: Vec<_> = branches
//             .into_iter()
//             .map(|(label, stmts)| {
//                 let label = self.resolve_expression(label).1;
//                 let stmts: Vec<_> = stmts
//                     .into_iter()
//                     .map(|stmt| self.resolve_statement(stmt))
//                     .collect();
//                 (label, stmts)
//             })
//             .collect();
//         let default = match default {
//             Some(stmts) => {
//                 let stmts: Vec<_> = stmts
//                     .into_iter()
//                     .map(|stmt| self.resolve_statement(stmt))
//                     .collect();
//                 Some(stmts)
//             }
//             None => None,
//         };
//         Statement::Switch {
//             expression,
//             branches,
//             default,
//             location,
//         }
//     }

//     /// Return a 3-tuple containing
//     /// - the type of this expression (using dummy type objects)
//     /// - the expression itself
//     /// - name of the identifier if the type of this expression is `Expression::Ident`
//     ///   and the type of the identifier is `T` (generic identifier).
//     fn resolve_expression(
//         &mut self,
//         expression: Expression<'a>,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let location = expression.locate();
//         match expression {
//             Expression::Ident { value, .. } => self.resolve_expression_ident(value, location),
//             Expression::IntConst { value, .. } => self.resolve_expression_intconst(value, location),
//             Expression::FloatConst { value, .. } => {
//                 self.resolve_expression_floatconst(value, location)
//             }
//             Expression::CharConst { value, .. } => {
//                 let dummy_char = Type::make_dummy("Char", false, false, false);
//                 (dummy_char, Expression::CharConst { value, location }, None)
//             }
//             Expression::StrConst { value, .. } => {
//                 let dummy_char_pointer = Type::make_dummy("Char", true, false, true);
//                 (
//                     dummy_char_pointer,
//                     Expression::StrConst { value, location },
//                     None,
//                 )
//             }
//             Expression::Prefix {
//                 operator,
//                 expression,
//                 ..
//             } => self.resolve_expression_prefix(operator, *expression, location),
//             Expression::Infix {
//                 left,
//                 operator,
//                 right,
//             } => self.resolve_expression_infix(*left, operator, *right, location),
//             Expression::Suffix {
//                 operator,
//                 expression,
//             } => self.resolve_expression_suffix(operator, *expression, location),
//             Expression::Index { expression, index } => {
//                 self.resolve_expression_index(*expression, *index, location)
//             }
//             Expression::Call {
//                 expression,
//                 arguments,
//             } => self.resolve_expression_call(*expression, arguments, location),
//             Expression::InitList { expressions, .. } => {
//                 self.resolve_expression_initlist(expressions, location)
//             }
//         }
//     }

//     fn resolve_expression_ident(
//         &mut self,
//         value: &'a str,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         match self.symbol_table.get(value) {
//             Some(r#type) => {
//                 let mut generic_ident = None;
//                 if let Type::T { .. } = r#type {
//                     generic_ident = Some(value);
//                 }
//                 (r#type, Expression::Ident { value, location }, generic_ident)
//             }
//             None => {
//                 let message = format!("Undefined ident `{}`.", value);
//                 self.push_error(&message, location);
//                 (dummy_t, Expression::Ident { value, location }, None)
//             }
//         }
//     }

//     fn resolve_expression_intconst(
//         &mut self,
//         value: i128,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let dummy_short_signed = Type::make_dummy("Short", true, false, false);
//         let dummy_short_unsigned = Type::make_dummy("Short", false, false, false);
//         let dummy_int_signed = Type::make_dummy("Int", true, false, false);
//         let dummy_int_unsigned = Type::make_dummy("Int", false, false, false);
//         let dummy_long_signed = Type::make_dummy("Long", true, false, false);
//         let dummy_long_unsigned = Type::make_dummy("Long", false, false, false);
//         let str_value = format!("{}", value);
//         if str_value.parse::<i16>().is_ok() {
//             return (
//                 dummy_short_signed,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<u16>().is_ok() {
//             return (
//                 dummy_short_unsigned,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<i32>().is_ok() {
//             return (
//                 dummy_int_signed,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<u32>().is_ok() {
//             return (
//                 dummy_int_unsigned,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<i64>().is_ok() {
//             return (
//                 dummy_long_signed,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<u64>().is_ok() {
//             return (
//                 dummy_long_unsigned,
//                 Expression::IntConst { value, location },
//                 None,
//             );
//         }
//         let message = format!("Range error for `{}`.", value);
//         self.push_error(&message, location);
//         (dummy_t, Expression::IntConst { value, location }, None)
//     }

//     fn resolve_expression_floatconst(
//         &mut self,
//         value: f64,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let dummy_float = Type::make_dummy("Float", false, false, false);
//         let dummy_double = Type::make_dummy("Double", false, false, false);
//         let str_value = format!("{}", value);
//         if str_value.parse::<f32>().is_ok() {
//             return (
//                 dummy_float,
//                 Expression::FloatConst { value, location },
//                 None,
//             );
//         }
//         if str_value.parse::<f64>().is_ok() {
//             return (
//                 dummy_double,
//                 Expression::FloatConst { value, location },
//                 None,
//             );
//         }
//         let message = format!("Range error for `{}`.", value);
//         self.push_error(&message, location);
//         (dummy_t, Expression::FloatConst { value, location }, None)
//     }

//     fn resolve_expression_prefix(
//         &mut self,
//         operator: &'a str,
//         expression: Expression<'a>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let dummy_int_signed = Type::make_dummy("Int", true, false, false);
//         let dummy_double = Type::make_dummy("Double", false, false, false);
//         let (r#type, expression, generic_ident) = self.resolve_expression(expression);
//         let expected_type = match operator {
//             "!" => dummy_int_signed,
//             "+" | "-" | "++" | "--" => dummy_double,
//             "*" | "&" => dummy_t,
//             _ => unreachable!(),
//         };
//         self.symbol_table
//             .update_generic_ident(generic_ident, expected_type);
//         if self.unify_types(expected_type, r#type).is_err() {
//             let message = format!(
//                 "Expect `{}` after `{}`, get `{}`.",
//                 expected_type, operator, r#type
//             );
//             self.push_error(&message, location);
//         }
//         let r#type = if operator == "*" {
//             r#type.set_pointer_flag(false)
//         } else if operator == "&" {
//             r#type.set_pointer_flag(true)
//         } else {
//             r#type
//         };
//         (
//             r#type,
//             Expression::Prefix {
//                 operator,
//                 expression: Box::new(expression),
//                 location,
//             },
//             None,
//         )
//     }

//     fn resolve_expression_infix(
//         &mut self,
//         left: Expression<'a>,
//         operator: &'a str,
//         right: Expression<'a>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let dummy_int_signed = Type::make_dummy("Int", true, false, false);
//         let dummy_double = Type::make_dummy("Double", false, false, false);
//         let (type_left, expr_left, generic_ident_left) = self.resolve_expression(left);
//         let (type_right, expr_right, generic_ident_right) = self.resolve_expression(right);
//         let r#type = match operator {
//             "=" => match self.unify_types(type_left, type_right) {
//                 Ok((type_left, _)) => type_left,
//                 Err(_) => {
//                     let message =
//                         format!("`{}` and `{}` cannot be unified.", type_left, type_right);
//                     self.push_error(&message, location);
//                     dummy_t
//                 }
//             },
//             "+=" | "-=" | "*=" | "/=" | "%=" | "+" | "-" | "*" | "/" | "%" => {
//                 if self.unify_types(dummy_double, type_right).is_err() {
//                     let message = format!("Expect a number after `{}`.", operator);
//                     self.push_error(&message, location);
//                     dummy_t
//                 } else {
//                     match self.unify_types(type_left, type_right) {
//                         Ok((type_left, _)) => type_left,
//                         Err(_) => {
//                             let message =
//                                 format!("`{}` and `{}` cannot be unified.", type_left, type_right);
//                             self.push_error(&message, location);
//                             dummy_t
//                         }
//                     }
//                 }
//             }
//             "||" | "&&" => {
//                 if self.unify_types(dummy_int_signed, type_right).is_err() {
//                     let message = format!("Expect {} after `{}`.", dummy_int_signed, operator);
//                     self.push_error(&message, location);
//                     dummy_int_signed
//                 } else {
//                     match self.unify_types(dummy_int_signed, type_left) {
//                         Ok(_) => dummy_int_signed,
//                         Err(_) => {
//                             let message =
//                                 format!("Expect {} before `{}`.", dummy_int_signed, operator);
//                             self.push_error(&message, location);
//                             dummy_t
//                         }
//                     }
//                 }
//             }
//             "==" | "!=" | "<" | ">" | "<=" | ">=" => {
//                 if self.unify_types(type_left, type_right).is_err() {
//                     let message =
//                         format!("`{}` and `{}` cannot be unified.", type_left, type_right);
//                     self.push_error(&message, location);
//                 }
//                 dummy_int_signed
//             }
//             _ => unreachable!(),
//         };
//         self.symbol_table
//             .update_generic_ident(generic_ident_left, r#type);
//         self.symbol_table
//             .update_generic_ident(generic_ident_right, r#type);
//         (
//             r#type,
//             Expression::Infix {
//                 left: Box::new(expr_left),
//                 operator,
//                 right: Box::new(expr_right),
//             },
//             None,
//         )
//     }
//     fn resolve_expression_suffix(
//         &mut self,
//         operator: &'a str,
//         expression: Expression<'a>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let dummy_double = Type::make_dummy("Double", false, false, false);
//         let (r#type, expression, generic_ident) = self.resolve_expression(expression);
//         let r#type = match self.unify_types(dummy_double, r#type) {
//             Ok(_) => r#type,
//             Err(_) => {
//                 let message = format!("Expect a number before `{}`.", operator);
//                 self.push_error(&message, location);
//                 dummy_t
//             }
//         };
//         self.symbol_table
//             .update_generic_ident(generic_ident, r#type);
//         (
//             r#type,
//             Expression::Suffix {
//                 operator,
//                 expression: Box::new(expression),
//             },
//             None,
//         )
//     }

//     fn resolve_expression_index(
//         &mut self,
//         expression: Expression<'a>,
//         index: Expression<'a>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let (type_expr, expression, _) = self.resolve_expression(expression);
//         let (type_index, index, _) = self.resolve_expression(index);
//         let r#type = if type_expr.get_array().0 {
//             match type_index {
//                 Type::Char { .. } | Type::Short { .. } | Type::Int { .. } | Type::Long { .. } => {}
//                 _ => {
//                     let message = format!(
//                         "Expect an integer as the array index, get `{}`.",
//                         type_index
//                     );
//                     self.push_error(&message, location);
//                 }
//             }
//             type_expr.set_array(false, None)
//         } else {
//             let message = format!("Expect an array, get `{}`.", type_expr);
//             self.push_error(&message, location);
//             dummy_t
//         };
//         (
//             r#type,
//             Expression::Index {
//                 expression: Box::new(expression),
//                 index: Box::new(index),
//             },
//             None,
//         )
//     }

//     fn resolve_expression_call(
//         &mut self,
//         expression: Expression<'a>,
//         arguments: Vec<Expression<'a>>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let name = match expression {
//             Expression::Ident { value, .. } => value,
//             _ => panic!("The function name in a function call now must be a StrConst."),
//         };
//         let (return_type, parameters) = match self.symbol_table.functions.get(name) {
//             None => {
//                 let message = format!("Undefined function `{:?}`.", name);
//                 self.push_error(&message, location);
//                 return (
//                     dummy_t,
//                     Expression::Call {
//                         expression: Box::new(Expression::Ident {
//                             value: name,
//                             location,
//                         }),
//                         arguments,
//                     },
//                     None,
//                 );
//             }
//             Some((return_type, parameters)) => (*return_type, parameters.clone()),
//         };
//         let arguments: Vec<_> = arguments
//             .into_iter()
//             .zip(parameters.iter())
//             .map(|(arg, (param, type_param))| {
//                 let (type_arg, arg, _) = self.resolve_expression(arg);
//                 if self.unify_types(*type_param, type_arg).is_err() {
//                     let message = format!(
//                         "Expect `{}` for `{}`, get `{}`.",
//                         type_param, param, type_arg
//                     );
//                     self.push_error(&message, location);
//                 }
//                 arg
//             })
//             .collect();
//         (
//             return_type,
//             Expression::Call {
//                 expression: Box::new(Expression::Ident {
//                     value: name,
//                     location,
//                 }),
//                 arguments,
//             },
//             None,
//         )
//     }

//     fn resolve_expression_initlist(
//         &mut self,
//         expressions: Vec<Expression<'a>>,
//         location: Location,
//     ) -> (Type, Expression<'a>, Option<&'a str>) {
//         let dummy_t: Type = Type::make_dummy("T", false, false, false);
//         let mut err_flag = false;
//         let mut r#type = dummy_t.clone();
//         let mut generic_idents = Vec::new();
//         let expressions: Vec<_> = expressions
//             .into_iter()
//             .map(|expr| {
//                 let (typ, expr, generic_ident) = self.resolve_expression(expr);
//                 generic_idents.push(generic_ident);
//                 match self.unify_types(r#type, typ) {
//                     Ok((typ, _)) => {
//                         r#type = typ;
//                     }
//                     Err(_) => {
//                         err_flag = true;
//                         let message = format!(
//                             "Inconsistent types in the init list: `{}` and `{}`.",
//                             r#type, typ
//                         );
//                         self.push_error(&message, location);
//                     }
//                 }
//                 expr
//             })
//             .collect();
//         if err_flag {
//             (
//                 dummy_t.set_array(true, Some(expressions.len())),
//                 Expression::InitList {
//                     expressions,
//                     location,
//                 },
//                 None,
//             )
//         } else {
//             for generic_ident in generic_idents {
//                 self.symbol_table
//                     .update_generic_ident(generic_ident, r#type);
//             }
//             (
//                 r#type.set_array(true, Some(expressions.len())),
//                 Expression::InitList {
//                     expressions,
//                     location,
//                 },
//                 None,
//             )
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::lexer::Lexer;
//     use crate::parser::Parser;

//     #[test]
//     fn multiple_functions() {
//         let source = "void f() {}\nvoid f() {}";
//         let expected_errors = vec![Error::Resolving {
//             message: "Multiple function definitions.".to_string(),
//             location: Location::empty(),
//         }];
//         let expected_ast = vec![
//             Function {
//                 r#type: Type::Void {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//                 name: "f",
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![],
//                     location: Location::empty(),
//                 },
//                 location: Location::empty(),
//             },
//             Function {
//                 r#type: Type::Void {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//                 name: "f",
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![],
//                     location: Location::empty(),
//                 },
//                 location: Location::empty(),
//             },
//         ];
//         let errors = Vec::new();
//         let (tokens, errors) = Lexer::new(&source, errors).run();
//         let (generic_ast, errors) = Parser::new(tokens, errors).run();
//         let (ast, errors) = Resolver::new(generic_ast, errors).run();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn unresolved() {
//         let source = "T f(T a) {}";
//         let expected_errors = vec![
//             Error::Resolving {
//                 message: "Unresolved return type of function `f`.".to_string(),
//                 location: Location::empty(),
//             },
//             Error::Resolving {
//                 message: "Unresolved parameter `a` in function `f`.".to_string(),
//                 location: Location::empty(),
//             },
//         ];
//         let expected_ast = vec![Function {
//             r#type: Type::T {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 location: Location::empty(),
//             },
//             name: "f",
//             parameters: [(
//                 "a",
//                 Type::T {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//             )]
//             .iter()
//             .cloned()
//             .collect(),
//             body: Statement::Block {
//                 statements: vec![],
//                 location: Location::empty(),
//             },
//             location: Location::empty(),
//         }];
//         let errors = Vec::new();
//         let (tokens, errors) = Lexer::new(&source, errors).run();
//         let (generic_ast, errors) = Parser::new(tokens, errors).run();
//         let (ast, errors) = Resolver::new(generic_ast, errors).run();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn forward_resolve() {
//         let source = "
//             f(int a) {
//                 b = &a;
//                 c = *b;
//                 T d[1] = { 1 };
//                 return d[a];
//             }

//             main() {
//                 a = f();
//                 return;
//             }
//         ";
//         let expected_errors = vec![];
//         let expected_ast = vec![
//             Function {
//                 r#type: Type::Short {
//                     signed_flag: true,
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//                 name: "f",
//                 parameters: [(
//                     "a",
//                     Type::Int {
//                         signed_flag: true,
//                         array_flag: false,
//                         array_len: None,
//                         pointer_flag: false,
//                         location: Location::empty(),
//                     },
//                 )]
//                 .iter()
//                 .cloned()
//                 .collect(),
//                 body: Statement::Block {
//                     statements: vec![
//                         Statement::Def {
//                             declarators: vec![(
//                                 Type::Int {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: true,
//                                     location: Location::empty(),
//                                 },
//                                 "b",
//                                 Some(Expression::Prefix {
//                                     operator: "&",
//                                     expression: Box::new(Expression::Ident {
//                                         value: "a",
//                                         location: Location::empty(),
//                                     }),
//                                     location: Location::empty(),
//                                 }),
//                             )],
//                             location: Location::empty(),
//                         },
//                         Statement::Def {
//                             declarators: vec![(
//                                 Type::Int {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::empty(),
//                                 },
//                                 "c",
//                                 Some(Expression::Prefix {
//                                     operator: "*",
//                                     expression: Box::new(Expression::Ident {
//                                         value: "b",
//                                         location: Location::empty(),
//                                     }),
//                                     location: Location::empty(),
//                                 }),
//                             )],
//                             location: Location::empty(),
//                         },
//                         Statement::Def {
//                             declarators: vec![(
//                                 Type::Short {
//                                     signed_flag: true,
//                                     array_flag: true,
//                                     array_len: Some(1),
//                                     pointer_flag: false,
//                                     location: Location::empty(),
//                                 },
//                                 "d",
//                                 Some(Expression::InitList {
//                                     expressions: vec![Expression::IntConst {
//                                         value: 1,
//                                         location: Location::empty(),
//                                     }],
//                                     location: Location::empty(),
//                                 }),
//                             )],
//                             location: Location::empty(),
//                         },
//                         Statement::Return {
//                             expression: Some(Expression::Index {
//                                 expression: Box::new(Expression::Ident {
//                                     value: "d",
//                                     location: Location::empty(),
//                                 }),
//                                 index: Box::new(Expression::Ident {
//                                     value: "a",
//                                     location: Location::empty(),
//                                 }),
//                             }),
//                             location: Location::empty(),
//                         },
//                     ],
//                     location: Location::empty(),
//                 },
//                 location: Location::empty(),
//             },
//             Function {
//                 r#type: Type::Void {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//                 name: "main",
//                 parameters: IndexMap::new(),
//                 body: Statement::Block {
//                     statements: vec![
//                         Statement::Def {
//                             declarators: vec![(
//                                 Type::Short {
//                                     signed_flag: true,
//                                     array_flag: false,
//                                     array_len: None,
//                                     pointer_flag: false,
//                                     location: Location::empty(),
//                                 },
//                                 "a",
//                                 Some(Expression::Call {
//                                     expression: Box::new(Expression::Ident {
//                                         value: "f",
//                                         location: Location::empty(),
//                                     }),
//                                     arguments: vec![],
//                                 }),
//                             )],
//                             location: Location::empty(),
//                         },
//                         Statement::Return {
//                             expression: None,
//                             location: Location::empty(),
//                         },
//                     ],
//                     location: Location::empty(),
//                 },
//                 location: Location::empty(),
//             },
//         ];
//         let errors = Vec::new();
//         let (tokens, errors) = Lexer::new(&source, errors).run();
//         let (generic_ast, errors) = Parser::new(tokens, errors).run();
//         let (ast, errors) = Resolver::new(generic_ast, errors).run();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }

//     #[test]
//     fn backward_resolve() {
//         let source = "double f(a) { return a; }";
//         let expected_errors = vec![];
//         let expected_ast = vec![Function {
//             r#type: Type::Double {
//                 array_flag: false,
//                 array_len: None,
//                 pointer_flag: false,
//                 location: Location::empty(),
//             },
//             name: "f",
//             parameters: [(
//                 "a",
//                 Type::Double {
//                     array_flag: false,
//                     array_len: None,
//                     pointer_flag: false,
//                     location: Location::empty(),
//                 },
//             )]
//             .iter()
//             .cloned()
//             .collect(),
//             body: Statement::Block {
//                 statements: vec![Statement::Return {
//                     expression: Some(Expression::Ident {
//                         value: "a",
//                         location: Location::empty(),
//                     }),
//                     location: Location::empty(),
//                 }],
//                 location: Location::empty(),
//             },
//             location: Location::empty(),
//         }];
//         let errors = Vec::new();
//         let (tokens, errors) = Lexer::new(&source, errors).run();
//         let (generic_ast, errors) = Parser::new(tokens, errors).run();
//         let (ast, errors) = Resolver::new(generic_ast, errors).run();
//         assert_eq!(errors, expected_errors);
//         assert_eq!(ast, expected_ast);
//     }
// }
