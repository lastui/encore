use std::collections::HashSet;

pub struct ParserState {
    pub in_strict_mode: bool,
    pub labels: HashSet<Box<str>>, 
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            in_strict_mode: false,
            labels: HashSet::new(),
        }
    }
}
