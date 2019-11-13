pub fn get_library(name: &str) -> Option<&str> {
    match name {
        "_test" => Some("_test"),
        "assert.h" => Some(ASSERT_H),
        _ => None,
    }
}

const ASSERT_H: &str = "
    void assert(int expression);
";
