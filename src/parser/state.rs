use std::collections::HashSet;

pub struct ParserState {
    pub in_strict_mode: bool,
    pub allow_yield: bool,
    pub allow_await: bool,
    pub in_loop: bool,
    pub in_switch: bool,
    pub in_function: bool,
    pub labels: HashSet<Box<str>>,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            in_strict_mode: false,
            allow_yield: false,
            allow_await: false,
            in_loop: false,
            in_switch: false,
            in_function: false,
            labels: HashSet::new(),
        }
    }
}