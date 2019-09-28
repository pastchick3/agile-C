use std::env;
use std::fs;

use agile_c::Transpiler;

fn main() {
    // cargo run -- ..\test_c\input.c ..\test_c\output.c
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("Input filename and output filename required!");
    }
    let source = fs::read_to_string(&args[1]).unwrap();
    let transpiler = Transpiler::new(&source);
    let transformed_source = transpiler.run();
    fs::write(&args[2], &transformed_source).unwrap();
}
