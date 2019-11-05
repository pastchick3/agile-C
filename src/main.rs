use std::fs;
use std::path::PathBuf;
use std::process;

use structopt::StructOpt;

use agile_c::Transpiler;

#[derive(StructOpt, Debug)]
#[structopt(name = "Agile C")]
struct Opt {
    /// The path to the input file.
    #[structopt(short, long, parse(from_os_str))]
    input: PathBuf,

    /// The path to the output file.
    #[structopt(short, long, parse(from_os_str))]
    output: PathBuf,
}

fn main() {
    let opt = Opt::from_args();
    let source = fs::read_to_string(&opt.input).unwrap_or_else(|err| {
        eprintln!("Fail to read the input file: {}.", err);
        process::exit(1);
    });
    let file_name = opt.input.file_name().unwrap().to_str().unwrap();
    let mut transpiler = Transpiler::new(file_name, &source).unwrap_or_else(|err| {
        eprintln!("Fail to construct the transpiler: {}.", err);
        process::exit(1);
    });
    let transformed_source = transpiler.run().unwrap_or_else(|err| {
        eprintln!("Fail to transform the source: {}", err);
        process::exit(1);
    });
    fs::write(opt.output, &transformed_source).unwrap_or_else(|err| {
        eprintln!("Fail to write the output file: {}.", err);
        process::exit(1);
    });
}
