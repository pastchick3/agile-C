pub fn get_library(name: &str) -> Option<String> {
    match name {
        "_test" => Some("_test".to_string()),
        "assert.h" => Some(
            "
            void assert(int expression);
        "
            .to_string(),
        ),
        _ => None,
    }
}
