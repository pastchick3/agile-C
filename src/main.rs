use std::fs;
use std::path::PathBuf;
use std::process;

use colored::*;
use structopt::StructOpt;

use agile_c::Transpiler;

#[derive(StructOpt)]
#[structopt(name = "agile_c")]
struct Opt {
    /// The path to the input file.
    #[structopt(short, long, parse(from_os_str))]
    input: PathBuf,

    /// The path to the output file.
    #[structopt(short, long, parse(from_os_str))]
    output: PathBuf,
}

fn main() {
    // Process command line arguments.
    let opt = Opt::from_args();
    // Read the input file as a string.
    let source = fs::read_to_string(&opt.input).unwrap_or_else(|err| {
        eprintln!("{}: {}", "Input File Error".red(), err);
        process::exit(1);
    });
    // Extract the file name of the input file.
    // We directly unwrap results because we have succesfully opened the file.
    let file_name = opt.input.file_name().unwrap().to_str().unwrap();

    // Construct the transpiler.
    let mut transpiler = Transpiler::new(file_name, &source).unwrap_or_else(|err| {
        eprintln!("{}: {}", "Transpiler Error".red(), err);
        process::exit(1);
    });
    // Perform the transpilation.
    let transformed_source = transpiler.run().unwrap_or_else(|err| {
        eprintln!("{}: {}", "Transpilation Error".red(), err);
        process::exit(1);
    });

    // Write the output file.
    fs::write(opt.output, &transformed_source).unwrap_or_else(|err| {
        eprintln!("{}: {}", "Output File Error".red(), err);
        process::exit(1);
    });
}
