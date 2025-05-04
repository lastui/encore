#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl LexerError {
    #[inline]
    pub fn new(message: &str, line: usize, column: usize) -> Self {
        LexerError {
            message: message.to_string(),
            line,
            column,
        }
    }
}

impl From<LexerError> for String {
    #[inline]
    fn from(error: LexerError) -> Self {
        error.message.clone()
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LexerError at line {}, column {}: {}", self.line, self.column, self.message)
    }
}

impl std::error::Error for LexerError {}