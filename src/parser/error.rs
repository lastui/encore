use crate::lexer::LexerError;
use std::fmt;

/// Represents an error that occurred during parsing
#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl ParserError {
    pub fn new(message: &str, line: usize, column: usize) -> Self {
        ParserError {
            message: message.to_string(),
            line,
            column,
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParserError at line {}, column {}: {}", self.line, self.column, self.message)
    }
}

impl std::error::Error for ParserError {}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        ParserError {
            message: error.message,
            line: error.line,
            column: error.column,
        }
    }
}

/// Type alias for parser results
pub type ParseResult<T> = Result<T, ParserError>;