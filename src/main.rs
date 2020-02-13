use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::{self, Command};
use std::time::SystemTime;

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
    #[structopt(name = "gcc", long)]
    gcc_flag: bool,

    /// Arguments to GCC.
    #[structopt(name = "args", subcommand)]
    gcc_args: Option<Args>,
}

#[derive(Debug, StructOpt)]
enum Args {
    #[structopt(external_subcommand)]
    Other(Vec<String>),
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

    if opt.gcc_flag {
        // Write the temporary input file and change file names into `String`.
        let mut temp = env::temp_dir();
        let stamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis();
        temp.push(&format!("_agile_c_temp_{}.c", stamp));
        fs::write(&temp, &transformed_source).unwrap_or_else(|err| {
            eprintln!("{}: {}", "Temp File Error".red(), err);
            process::exit(1);
        });
        let temp = temp.into_os_string().into_string().unwrap_or_else(|_| {
            eprintln!("{}: Invalid Unicode data.", "Temp File Error".red());
            process::exit(1);
        });
        let output = opt
            .output
            .into_os_string()
            .into_string()
            .unwrap_or_else(|_| {
                eprintln!("{}: Invalid Unicode data.", "Output File Error".red());
                process::exit(1);
            });

        // Assemble arguments to GCC.
        let args = if let Some(Args::Other(mut args)) = opt.gcc_args {
            args.remove(0); // Remove "args".
            args.extend(vec!["-o".to_string(), output, temp.clone()]);
            args
        } else {
            vec!["-o".to_string(), output, temp.clone()]
        };

        // Invoke GCC.
        let status = Command::new("gcc")
            .args(&args)
            .status()
            .unwrap_or_else(|_| {
                eprintln!("{}: Cannot execute GCC.", "GCC Error".red());
                process::exit(1);
            });

        // Clean up.
        fs::remove_file(&temp).unwrap_or_else(|err| {
            eprintln!("{}: {}", "Temp File Error".red(), err);
            process::exit(1);
        });
        if !status.success() {
            process::exit(1);
        }
    } else {
        // Write the output file.
        fs::write(opt.output, &transformed_source).unwrap_or_else(|err| {
            eprintln!("{}: {}", "Output File Error".red(), err);
            process::exit(1);
        });
    }
}
