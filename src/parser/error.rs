use crate::lexer::{LexerError, Token};
use super::core::Parser;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub source_line: Option<String>,
    pub source_span: Option<(usize, usize)>,
    pub context_stack: Vec<String>,
    pub current_token: Option<Token>,
}

impl ParserError {

    pub fn new(message: &str, line: usize, column: usize) -> Self {
        ParserError {
            message: message.to_string(),
            line,
            column,
            source_line: None,
            source_span: None,
            context_stack: Vec::new(),
            current_token: None,
        }
    }
    
    /// Create a parser error from a parser reference and token information
    pub fn from_parser(parser: &Parser, message: &str, line: usize, column: usize, token_length: usize) -> Self {
        let source = parser.get_source_text();
        
        let source_line = extract_source_line_with_context(&source, line, column, 60);
        let span_end = column + token_length;

        let (adjusted_column, adjusted_span_end) = if source_line.starts_with("...") {
            let adjusted_col = column.min(60) + 3;
            let adjusted_end = adjusted_col + token_length;
            (adjusted_col, adjusted_end)
        } else {
            (column, span_end)
        };

        let context_stack = parser.get_context_stack_info();
            
        let current_token = parser.peek_token().cloned();

        ParserError {
            message: message.to_string(),
            line,
            column,
            source_line: Some(source_line),
            source_span: Some((adjusted_column, adjusted_span_end)),
            context_stack,
            current_token,
        }
    }
    
    /// Create a parser error from the current token with an immutable reference
    pub fn at_current(parser: &Parser, message: &str) -> Self {
        if let Some(token) = parser.peek_token() {
            Self::from_parser(
                parser,
                message,
                token.line,
                token.column,
                token.length
            )
        } else if let Some(token) = parser.previous() {
            Self::from_parser(
                parser,
                message,
                token.line,
                token.column,
                token.length
            )
        } else {
            Self::new(message, 0, 0)
        }
    }
    
    /// Create a parser error from the current token with a mutable reference
    pub fn at_current_mut(parser: &mut Parser, message: &str) -> Self {
        Self::at_current(&*parser, message)
    }
    
    /// Create a parser error from the previous token with an immutable reference
    pub fn at_previous(parser: &Parser, message: &str) -> Self {
        if let Some(token) = parser.previous() {
            Self::from_parser(
                parser,
                message,
                token.line,
                token.column,
                token.length
            )
        } else if let Some(token) = parser.peek_token() {
            Self::from_parser(
                parser,
                message,
                token.line,
                token.column,
                token.length
            )
        } else {
            // Fallback if no token is available
            Self::new(message, 0, 0)
        }
    }
    
    /// Create a parser error from the previous token with a mutable reference
    pub fn at_previous_mut(parser: &mut Parser, message: &str) -> Self {
        Self::at_previous(&*parser, message)
    }

}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}", self.message)?;
        
        if let Some(source_line) = &self.source_line {
            // Calculate width needed for line numbers
            let line_width = num_digits(self.line);
            
            // Show location information
            writeln!(f, " --> line {}, column {}", self.line, self.column)?;
            writeln!(f, "{:width$}|", "", width = line_width + 1)?;
            
            // Show the error line with context
            writeln!(f, "{:>width$} | {}", self.line, source_line, width = line_width)?;
            
            // Print the error indicator
            let (start, end) = self.source_span.unwrap_or((self.column, self.column + 1));
            write!(f, "{:width$} | ", "", width = line_width)?;
            
            // Print spaces up to the start position
            for _ in 0..start {
                write!(f, " ")?;
            }
            
            // Calculate how many carets to print (limited by the actual line length)
            let visible_end = if let Some(line) = &self.source_line {
                end.min(start + line.len() - start.min(line.len()))
            } else {
                end
            };
            
            // Print carets for the span length
            for _ in start..visible_end.max(start+1) {
                write!(f, "^")?;
            }
            
            writeln!(f)?;
            
            // Print current token information if available
            if let Some(token) = &self.current_token {
                writeln!(f, "\nCurrent token: {:#?}", token.token_type)?;
            }
            
            // Print context stack information if available
            if !self.context_stack.is_empty() {
                writeln!(f, "\nLexical context stack (newest first):")?;
                for (i, context) in self.context_stack.iter().enumerate() {
                    writeln!(f, "  {}: {}", i, context)?;
                }
            }
        } else {
            writeln!(f, "at line {}, column {}", self.line, self.column)?;
            
            // Print current token information if available
            if let Some(token) = &self.current_token {
                writeln!(f, "\nCurrent token: {:#?}", token.token_type)?;
            }
            
            // Print context stack information if available
            if !self.context_stack.is_empty() {
                writeln!(f, "\nLexical context stack (newest first):")?;
                for (i, context) in self.context_stack.iter().enumerate() {
                    writeln!(f, "  {}: {}", i, context)?;
                }
            }
        }
        
        Ok(())
    }
}

/// Helper function to calculate the number of digits in a number
#[inline]
fn num_digits(n: usize) -> usize {
    if n == 0 {
        return 1;
    }
    let mut count = 0;
    let mut num = n;
    while num > 0 {
        count += 1;
        num /= 10;
    }
    count
}

impl std::error::Error for ParserError {}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        ParserError {
            message: error.message,
            line: error.line,
            column: error.column,
            source_line: None,
            source_span: None,
            context_stack: Vec::new(),
            current_token: None,
        }
    }
}

/// Extract a specific line from source code with limited context around the error position
#[inline]
fn extract_source_line_with_context(source: &str, line_number: usize, column: usize, context_size: usize) -> String {
    let line = source.lines()
                    .nth(line_number - 1)
                    .unwrap_or("");
    
    if line.len() <= context_size * 2 {
        // Line is short enough to show in full
        return line.to_string();
    }
    
    // Calculate start and end positions with context
    let start = if column > context_size {
        column - context_size
    } else {
        0
    };
    
    let end = if column + context_size < line.len() {
        column + context_size
    } else {
        line.len()
    };
    
    // Create the context string with ellipses as needed
    let mut result = String::with_capacity(context_size * 2 + 6); // +6 for possible ellipses
    
    if start > 0 {
        result.push_str("...");
    }
    
    // Get the substring with proper UTF-8 character boundaries
    let context_str = line.chars()
                         .skip(start)
                         .take(end - start)
                         .collect::<String>();
    result.push_str(&context_str);
    
    if end < line.len() {
        result.push_str("...");
    }
    
    result
}

pub type ParseResult<T> = Result<T, ParserError>;

#[macro_export]
macro_rules! parser_error_at_current {
    ($self:expr, $message:expr) => {
        $crate::parser::error::ParserError::at_current($self, $message)
    };
    ($self:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::parser::error::ParserError::at_current($self, &format!($fmt, $($arg)*))
    };
}

#[macro_export]
macro_rules! parser_error_at_previous {
    ($self:expr, $message:expr) => {
        $crate::parser::error::ParserError::at_previous($self, $message)
    };
    ($self:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::parser::error::ParserError::at_previous($self, &format!($fmt, $($arg)*))
    };
}

#[macro_export]
macro_rules! parser_error_at_current_mut {
    ($self:expr, $message:expr) => {
        $crate::parser::error::ParserError::at_current_mut($self, $message)
    };
    ($self:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::parser::error::ParserError::at_current_mut($self, &format!($fmt, $($arg)*))
    };
}

#[macro_export]
macro_rules! parser_error_at_previous_mut {
    ($self:expr, $message:expr) => {
        $crate::parser::error::ParserError::at_previous_mut($self, $message)
    };
    ($self:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::parser::error::ParserError::at_previous_mut($self, &format!($fmt, $($arg)*))
    };
}
