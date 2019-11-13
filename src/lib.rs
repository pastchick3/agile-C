//! A type inference transpiler for C programming language

mod cstdlib;
mod lexer;
mod parser;
mod preprocessor;
mod resolver;
mod serializer;
mod structure;

use lexer::Lexer;
use parser::Parser;
use preprocessor::Preprocessor;
use resolver::Resolver;
use serializer::Serializer;
use structure::Error;

/// The main struct that users should use.
///
/// # Examples
///
/// ```
/// use agile_c::Transpiler;
///
/// let input = "
///     func(a) {
///         b = 1.1;
///         return a + b;
///     }
/// ";
///
/// let expected_output =
/// "float func(float a) {
///     float b = 1.1;
///     return a + b;
/// }
/// ";
///
/// // let transpiler = Transpiler::new(input).unwrap();
/// // let output = transpiler.run().unwrap();
/// // assert_eq!(expected_output, output);
/// ```
pub struct Transpiler {
    file_name: String,
    source: String,
    errors: Vec<Error>,
}

impl Transpiler {
    /// Instantiate a transpiler.
    ///
    /// # Errors
    ///
    /// This function will return an error string if the source is empty.
    pub fn new(file_name: &str, source: &str) -> Result<Transpiler, String> {
        if source.is_empty() {
            Err("Empty source file is not allowed.".to_string())
        } else {
            Ok(Transpiler {
                file_name: file_name.to_string(),
                source: source.to_string(),
                errors: Vec::new(),
            })
        }
    }

    /// Run the transpilation process, and return the transpiled program.
    ///
    /// # Errors
    ///
    /// This function will return an error string if transpilation fails.
    pub fn run(&mut self) -> Result<String, String> {
        let lines = Preprocessor::new(&self.file_name, &self.source, &mut self.errors)
            .run()
            .map_err(|_| self.format_errors())?;
        let tokens = Lexer::new(lines, &mut self.errors)
            .run()
            .map_err(|_| self.format_errors())?;
        let generic_ast = Parser::new(tokens, &mut self.errors)
            .run()
            .map_err(|_| self.format_errors())?;
        let ast = Resolver::new(generic_ast, &mut self.errors)
            .run()
            .map_err(|_| self.format_errors())?;
        let transformed_source = Serializer::new(ast).run();
        Ok(transformed_source)
    }

    fn format_errors(&self) -> String {
        let mut message = self.errors.iter().fold("\n".to_string(), |mut msg, err| {
            msg.push_str(&format!("    {}\n", err));
            msg
        });
        message.pop();
        message
    }
}
