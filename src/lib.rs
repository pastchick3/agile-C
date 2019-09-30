mod structure;
mod lexing;

pub struct Transpiler {
    source: String,
}

impl Transpiler {
    pub fn new(source: &str) -> Transpiler {
        Transpiler {
            source: source.to_string(),
        }
    }

    pub fn run(&self) -> Result<String, String> {
        let mut errors = Vec::new();
        let token_stream = lexing::run(&self.source, &mut errors);
        if errors.len() == 0 {
            Ok(format!("{:#?}", token_stream))
        } else {
            Err(format!("{:#?}", errors))
        }
    }
}
