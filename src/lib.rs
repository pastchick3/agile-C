mod cstdlib;
mod lexer;
mod parser;
mod preprocessor;
// mod resolver;
// mod serializer;
mod structure;

use lexer::Lexer;
use parser::Parser;
use preprocessor::Preprocessor;
// use resolver::Resolver;
// use serializer::Serializer;
use structure::Error;

/// A type inference transpiler for the C programming language.
///
/// # Examples
///
/// ```
/// use agile_c::Transpiler;
///
/// let input = "
///     func() {
///         a = 1.1;
///         return a;
///     }
/// ";
///
/// let expected_output =
/// "float func() {
///     float a = 1.1;
///     return a;
/// }
/// ";
///
/// let mut transpiler = Transpiler::new("test", input).unwrap();
/// let output = transpiler.run().unwrap();
/// assert_eq!(output, expected_output);
/// ```
pub struct Transpiler {
    file_name: String,  // the file name of the input file
    source: String,     // the source string of the input file
    errors: Vec<Error>, // a vector containing all errors encounted during transpilation
}

impl Transpiler {
    /// Instantiate a transpiler.
    ///
    /// # Errors
    ///
    /// This function will return an error string if the source is empty.
    pub fn new(file_name: &str, source: &str) -> Result<Transpiler, String> {
        if source.is_empty() {
            Err("Empty source file.".to_string())
        } else {
            Ok(Transpiler {
                file_name: file_name.to_string(),
                source: source.to_string(),
                errors: Vec::new(),
            })
        }
    }

    /// Perform the transpilation, and return the transpiled program.
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
        // let ast = Resolver::new(generic_ast, &mut self.errors)
        //     .run()
        //     .map_err(|_| self.format_errors())?;
        // let transformed_source = Serializer::new(ast).run();
        // Ok(transformed_source)
        Ok(String::new())
    }

    /// Format errors into a string.
    fn format_errors(&self) -> String {
        let mut message = self.errors.iter().fold("\n".to_string(), |mut msg, err| {
            msg.push_str(&format!("    {}\n", err));
            msg
        });
        message.pop(); // Pop the last "\n".
        message
    }
}
