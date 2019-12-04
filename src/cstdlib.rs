/// Get a C standard library as a string.
///
/// Not all C standard libraries are added here, and there are only
/// declarations included. The only purpose is to perform type checking
/// and type inference.
pub fn get_library(name: &str) -> Option<&str> {
    match name {
        "_test" => Some("_test"), // for unit test
        "assert.h" => Some(ASSERT_H),
        "stdio.h" => Some(STDIO_H),
        _ => None,
    }
}

const ASSERT_H: &str = "
    void assert(int expression);
";

const STDIO_H: &str = "
    int printf(char *format, ...);
";
