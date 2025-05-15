use crate::lexer::{LexerError, TemplatePart, Token};
use super::parser::Parser;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub source_line: Option<String>,
    pub source_span: Option<(usize, usize)>,
    pub context_stack: Vec<String>,
    pub token_stack: Vec<String>,
}

impl ParserError {

    pub fn new(parser: &Parser, message: &str) -> Self {

        let context_stack = parser.get_context_stack_info();

        let token_stack = parser.get_token_stack_info();
            
        let token = parser.peek();

        // Infer token length based on its type
        let token_length = match token {

            Token::EOS => 0,

            Token::LeftParen
            | Token::RightParen
            | Token::LeftBrace
            | Token::RightBrace
            | Token::LeftBracket
            | Token::RightBracket
            | Token::Comma
            | Token::Dot
            | Token::Semicolon
            | Token::Colon
            | Token::Question
            | Token::Hash
            | Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Percent
            | Token::Equal
            | Token::Bang
            | Token::Greater
            | Token::Caret
            | Token::Less
            | Token::Pipe
            | Token::Ampersand
            | Token::Tilde => 1,

            Token::PlusPlus
            | Token::PlusEqual
            | Token::MinusMinus
            | Token::MinusEqual
            | Token::StarEqual
            | Token::SlashEqual
            | Token::PercentEqual
            | Token::EqualEqual
            | Token::BangEqual
            | Token::GreaterEqual
            | Token::GreaterGreater
            | Token::LessEqual
            | Token::LessLess
            | Token::Arrow
            | Token::StarStar
            | Token::AmpersandEqual
            | Token::AmpersandAmpersand
            | Token::PipeEqual
            | Token::PipePipe
            | Token::CaretEqual
            | Token::QuestionQuestion
            | Token::If
            | Token::In
            | Token::Of
            | Token::Do
            | Token::As
            | Token::QuestionDot => 2,

            Token::EqualEqualEqual
            | Token::BangEqualEqual
            | Token::GreaterGreaterEqual
            | Token::GreaterGreaterGreater
            | Token::LessLessEqual
            | Token::AmpersandAmpersandEqual
            | Token::PipePipeEqual
            | Token::Ellipsis
            | Token::StarStarEqual
            | Token::Var            
            | Token::Let
            | Token::For
            | Token::New
            | Token::Try
            | Token::Get
            | Token::Set
            | Token::QuestionQuestionEqual => 3,

            Token::Null 
            | Token::GreaterGreaterGreaterEqual
            | Token::With
            | Token::Else
            | Token::Void
            | Token::This
            | Token::Case
            | Token::Eval
            | Token::Enum 
            | Token::From
            | Token::True => 4,

            Token::Const 
            | Token::While
            | Token::Break
            | Token::Super
            | Token::Await
            | Token::Class
            | Token::Throw
            | Token::Catch
            | Token::Yield
            | Token::Async
            | Token::False => 5,

            Token::Return 
            | Token::Export 
            | Token::Import
            | Token::Switch
            | Token::Typeof
            | Token::Target
            | Token::Public
            | Token::Delete
            | Token::Static => 6,

            Token::Extends 
            | Token::Default 
            | Token::Finally
            | Token::Package
            | Token::Private => 7,

            Token::Debugger 
            | Token::Continue
            | Token::Function => 8,

            Token::Undefined 
            | Token::Interface
            | Token::Protected
            | Token::Arguments => 9,

            Token::Implements 
            | Token::InstanceOf => 10,

            Token::Constructor => 11,

            // Literals
            Token::Identifier(ref name) => name.len(),
            Token::StringLiteral(ref value) => value.len() + 2, // Account for quotation marks
            Token::NumberLiteral(ref value) => value.to_string().len(),
            Token::BigIntLiteral(ref value) => value.len() + 1, // Account for the trailing 'n'
            Token::RegExpLiteral(ref pattern, ref flags) => pattern.len() + flags.len() + 2, // Account for the slashes
            Token::TemplateLiteral(ref parts) => parts.iter().fold(2, |acc, part| {
                acc + match part {
                    TemplatePart::String(s) => s.len(),
                    TemplatePart::Expression(e) => e.len(),
                }
            }),

        };

        let [line, column] = parser.get_current_position();

        let col = column - token_length;

        let source = parser.get_source_text();
        
        let source_line = extract_source_line_with_context(&source, line, col, 60);
        let span_end = column;

        let (adjusted_column, adjusted_span_end) = if source_line.starts_with("...") {
            let adjusted_col = col.min(60) + 3;
            let adjusted_end = adjusted_col + token_length;
            (adjusted_col, adjusted_end)
        } else {
            (col, span_end)
        };

        ParserError {
            message: message.to_string(),
            line,
            column: col,
            source_line: Some(source_line),
            source_span: Some((adjusted_column, adjusted_span_end)),
            context_stack,
            token_stack,
        }
    }
    
    /// Create a parser error from the current token with an immutable reference
    pub fn at_current(parser: &Parser, message: &str) -> Self {
        Self::new(parser, message)
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
        } else {
            writeln!(f, "at line {}, column {}", self.line, self.column)?;
        }

        if !self.token_stack.is_empty() {
            writeln!(f, "\nToken stack:")?;
            for (i, token) in self.token_stack.iter().enumerate() {
                writeln!(f, "  {}: {}", i, token)?;
            }
        }

        if !self.context_stack.is_empty() {
            writeln!(f, "\nLexical context stack:")?;
            for (i, context) in self.context_stack.iter().enumerate() {
                writeln!(f, "  {}: {}", i, context)?;
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
            token_stack: Vec::new(),
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
