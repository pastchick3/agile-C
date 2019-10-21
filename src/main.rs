use std::env;
use std::fs;
use std::process;

use agile_c::Transpiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("The input filename and the output filename are required.");
        process::exit(1);
    }
    let source = fs::read_to_string(&args[1]).unwrap_or_else(|err| {
        eprintln!("Fail to read the input file: {}.", err);
        process::exit(1);
    });
    let transpiler = Transpiler::new(&source).unwrap_or_else(|err| {
        eprintln!("Fail to construct the transpiler: {}.", err);
        process::exit(1);
    });
    let transformed_source = transpiler.run().unwrap_or_else(|err| {
        eprintln!("Fail to transform the source: {}", err);
        process::exit(1);
    });
    fs::write(&args[2], &transformed_source).unwrap_or_else(|err| {
        eprintln!("Fail to write the output file: {}.", err);
        process::exit(1);
    });
}
