use std::fs;
use std::path::PathBuf;
use std::process::{self, Command};

use colored::*;
use structopt::StructOpt;

use agile_c::Transpiler;

#[derive(Debug, StructOpt)]
#[structopt(name = "agile_c")]
struct Opt {
    /// The path to the input file.
    #[structopt(short, long, parse(from_os_str))]
    input: PathBuf,

    /// The path to the output file.
    #[structopt(short, long, parse(from_os_str))]
    output: PathBuf,

    /// Whether to invoke GCC.
    #[structopt(name="gcc", long)]
    gcc_flag: bool,

    /// Arguments to GCC.
    #[structopt(name="args", subcommand)]
    gcc_args: Option<Args>,
}

#[derive(Debug, StructOpt)]
enum Args {
    #[structopt(external_subcommand)]
    other(Vec<String>),
}

fn main() {
    // Process command line arguments.
    let opt = Opt::from_args();
    println!("{:?}", opt);
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

    if opt.gcc_flag {
        // Invoke GCC.
        todo!("invoke gcc with output and args"); 
        if let Some(args) = opt.gcc_args {

        } else {
        Command::new("gcc")
            .args(&["/C", "echo hello"])
            .output()
            .expect("failed to execute process");
        }
    } else {
        // Write the output file.
        fs::write(opt.output, &transformed_source).unwrap_or_else(|err| {
            eprintln!("{}: {}", "Output File Error".red(), err);
            process::exit(1);
        });
    }
}
