use std::env;
use std::fs;

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn hello_world_agile_c() {
    let source = "
        #include <stdio.h>

        int main(void) {{
            printf(\"Hello, World!\");
            return 0;
        }}
    ";
    let mut input = env::temp_dir();
    input.push("_hello_world_agile_c_input_temp.c");
    let mut output = env::temp_dir();
    output.push("_hello_world_agile_c_output_temp.c");
    fs::write(&input, source).unwrap();
    Command::cargo_bin("agile_c")
        .unwrap()
        .args(&[
            "--input",
            input.to_str().unwrap(),
            "--output",
            output.to_str().unwrap(),
        ])
        .assert()
        .success();
    fs::remove_file(&output).unwrap();
}

#[test]
fn hello_world_gcc() {
    let source = "
        #include <stdio.h>

        int main(void) {{
            printf(\"Hello, World!\");
            return 0;
        }}
    ";
    let mut input = env::temp_dir();
    input.push("_hello_world_gcc_input_temp.c");
    let mut output = env::temp_dir();
    output.push("_hello_world_gcc_output_temp.exe");
    fs::write(&input, source).unwrap();
    Command::cargo_bin("agile_c")
        .unwrap()
        .args(&[
            "--input",
            input.to_str().unwrap(),
            "--output",
            output.to_str().unwrap(),
            "--gcc",
        ])
        .assert()
        .success();
    fs::remove_file(&output).unwrap();
}

#[test]
fn hello_world_gcc_args() {
    let source = "
        #include <stdio.h>

        int main(void) {{
            printf(\"Hello, World!\");
            return 0;
        }}
    ";
    let mut input = env::temp_dir();
    input.push("_hello_world_gcc_args_input_temp.c");
    let mut output = env::temp_dir();
    output.push("_hello_world_gcc_args_output_temp.exe");
    fs::write(&input, source).unwrap();
    Command::cargo_bin("agile_c")
        .unwrap()
        .args(&[
            "--input",
            input.to_str().unwrap(),
            "--output",
            output.to_str().unwrap(),
            "--gcc",
            "args",
            "--version",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Free Software Foundation"));
}
