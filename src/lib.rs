mod lexer;
mod branched_parser;
mod name_resolver;
mod type_resolver;
mod serializer;

use std::fs;

use lexer::Lexer;
use branched_parser::BranchedParser;
use name_resolver::NameResolver;
use type_resolver::TypeResolver;
use serializer::Serializer;

pub struct Transpiler {
    source: String,
}

impl Transpiler {
    pub fn new(source: &str) -> Transpiler {
        Transpiler {
            source: String::from(source),
        }
    }

    pub fn run(&self) -> String {
        let lexer = Lexer::new(&self.source);
        let token_stream = lexer.run();
        fs::write("token_stream.c", &format!("{:#?}", &token_stream)).unwrap();
        let mut branched_parser = BranchedParser::new(token_stream);
        let branched_ast = branched_parser.run();
        fs::write("branched_ast.c", &format!("{:#?}", &branched_ast)).unwrap();
        let name_resolver = NameResolver::new();
        let generic_ast = name_resolver.run(branched_ast);
        fs::write("generic_ast.c", &format!("{:#?}", &generic_ast)).unwrap();
        let type_resolver = TypeResolver::new();
        let ast = type_resolver.run(generic_ast);
        fs::write("ast.c", &format!("{:#?}", &ast)).unwrap();
        let trans_source = Serializer::new().run(ast);
        // format!("{:#?}", &trans_source)
        trans_source
    }
}
