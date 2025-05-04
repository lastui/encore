use crate::lexer::LexerError;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub source_line: Option<String>,
    pub source_span: Option<(usize, usize)>,
}

impl ParserError {
    pub fn new(message: &str, line: usize, column: usize) -> Self {
        ParserError {
            message: message.to_string(),
            line,
            column,
            source_line: None,
            source_span: None,
        }
    }
        
    pub fn with_token_span(message: &str, line: usize, column: usize, token_length: usize, source: &str) -> Self {
        // Extract just the relevant line with limited context
        let source_line = extract_source_line_with_context(source, line, column, 60);
        let span_end = column + token_length;
        
        // Adjust column if we've added ellipsis at the start
        let (adjusted_column, adjusted_span_end) = if source_line.starts_with("...") {
            (column.min(60) + 3, span_end.min(60) + 3)
        } else {
            (column, span_end)
        };
        
        ParserError {
            message: message.to_string(),
            line,
            column,
            source_line: Some(source_line),
            source_span: Some((adjusted_column, adjusted_span_end)),
        }
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
            
            // Print carets for the span length
            for _ in start..end.max(start+1) {
                write!(f, "^")?;
            }
            
            writeln!(f)?;
        } else {
            writeln!(f, "at line {}, column {}", self.line, self.column)?;
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

/// Type alias for parser results
pub type ParseResult<T> = Result<T, ParserError>;
