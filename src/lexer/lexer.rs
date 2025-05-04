use std::collections::HashSet;
use crate::lexer::{Token, TokenType, TemplatePart, LexerError};

pub struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,   // TODO chars: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

macro_rules! add_token {
    ($self:expr, $token_type:expr) => {
        $self.tokens.push(Token::new($token_type, $self.line, $self.column - 1, 1))
    };
    ($self:expr, $token_type:expr, $length:expr) => {
        $self.tokens.push(Token::new($token_type, $self.line, $self.column - $length, $length))
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().collect(),
            source,
            tokens: Vec::with_capacity(source.len() / 4),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        let eof_column = self.column;
        add_token!(self, TokenType::EOF, 0);
        Ok(std::mem::take(&mut self.tokens))
    }
    
    fn scan_token(&mut self) -> Result<(), LexerError> {
        let c = self.advance();
        
        match c {
            '(' => add_token!(self, TokenType::LeftParen),
            ')' => add_token!(self, TokenType::RightParen),
            '{' => add_token!(self, TokenType::LeftBrace),
            '}' => add_token!(self, TokenType::RightBrace),
            '[' => add_token!(self, TokenType::LeftBracket),
            ']' => add_token!(self, TokenType::RightBracket),
            ',' => add_token!(self, TokenType::Comma),
            ';' => add_token!(self, TokenType::Semicolon),
            ':' => add_token!(self, TokenType::Colon),
            '#' => add_token!(self, TokenType::Hash),
            
            '.' => {
                if self.match_char('.') && self.match_char('.') {
                    add_token!(self, TokenType::Ellipsis, 3);
                } else {
                    add_token!(self, TokenType::Dot);
                }
            },                
            '+' => {
                if self.match_char('+') {
                    add_token!(self, TokenType::PlusPlus, 2);
                } else if self.match_char('=') {
                    add_token!(self, TokenType::PlusEqual, 2);
                } else {
                    add_token!(self, TokenType::Plus);
                }
            },
            '-' => {
                if self.match_char('-') {
                    add_token!(self, TokenType::MinusMinus, 2);
                } else if self.match_char('=') {
                    add_token!(self, TokenType::MinusEqual, 2);
                } else {
                    add_token!(self, TokenType::Minus);
                }
            },
            '%' => {
                if self.match_char('=') {
                    add_token!(self, TokenType::PercentEqual, 2);
                } else {
                    add_token!(self, TokenType::Percent);
                }
            },
            '^' => {
                if self.match_char('=') {
                    add_token!(self, TokenType::CaretEqual, 2);
                } else {
                    add_token!(self, TokenType::Caret);
                }
            },
            '*' => {
                if self.match_char('*') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::StarStarEqual, 3);
                    } else {
                        add_token!(self, TokenType::StarStar, 2);
                    }
                } else if self.match_char('=') {
                    add_token!(self, TokenType::StarEqual, 2);
                } else {
                    add_token!(self, TokenType::Star);
                }
            },
            '/' => self.handle_slash()?,
            '!' => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::BangEqualEqual, 3);
                    } else {
                        add_token!(self, TokenType::BangEqual, 2);
                    }
                } else {
                    add_token!(self, TokenType::Bang);
                }
            },
            '=' => {
                if self.match_char('>') {
                    add_token!(self, TokenType::Arrow, 2);
                } else if self.match_char('=') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::EqualEqualEqual, 3);
                    } else {
                        add_token!(self, TokenType::EqualEqual, 2);
                    }
                } else {
                    add_token!(self, TokenType::Equal);
                }
            },
            
            '<' => {
                if self.match_char('=') {
                    add_token!(self, TokenType::LessEqual, 2);
                } else if self.match_char('<') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::LessLessEqual, 3);
                    } else {
                        add_token!(self, TokenType::LessLess, 2);
                    }
                } else {
                    add_token!(self, TokenType::Less);
                }
            },
            
            '>' => {
                if self.match_char('=') {
                    add_token!(self, TokenType::GreaterEqual, 2);
                } else if self.match_char('>') {
                    if self.match_char('>') {
                        if self.match_char('=') {
                            add_token!(self, TokenType::GreaterGreaterGreaterEqual, 4);
                        } else {
                            add_token!(self, TokenType::GreaterGreaterGreater, 3);
                        }
                    } else if self.match_char('=') {
                        add_token!(self, TokenType::GreaterGreaterEqual, 3);
                    } else {
                        add_token!(self, TokenType::GreaterGreater, 2);
                    }
                } else {
                    add_token!(self, TokenType::Greater);
                }
            },
            
            '&' => {
                if self.match_char('&') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::AmpersandAmpersandEqual, 3);
                    } else {
                        add_token!(self, TokenType::AmpersandAmpersand, 2);
                    }
                } else if self.match_char('=') {
                    add_token!(self, TokenType::AmpersandEqual, 2);
                } else {
                    add_token!(self, TokenType::Ampersand);
                }
            },
            
            '|' => {
                if self.match_char('|') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::PipePipeEqual, 3);
                    } else {
                        add_token!(self, TokenType::PipePipe, 2);
                    }
                } else if self.match_char('=') {
                    add_token!(self, TokenType::PipeEqual, 2);
                } else {
                    add_token!(self, TokenType::Pipe);
                }
            },
            '~' => add_token!(self, TokenType::Tilde),            
            '?' => {
                if self.match_char('?') {
                    if self.match_char('=') {
                        add_token!(self, TokenType::QuestionQuestionEqual, 3);
                    } else {
                        add_token!(self, TokenType::QuestionQuestion, 2);
                    }
                } else if self.match_char('.') {
                    add_token!(self, TokenType::QuestionDot, 2);
                } else {
                    add_token!(self, TokenType::Question);
                }
            },
            
            '"' => self.string('"')?,
            '\'' => self.string('\'')?,
            '`' => self.template_literal()?,
            
            ' ' | '\r' | '\t' => {}, // Ignore whitespace
            '\n' => {
                self.line += 1;
                self.column = 0;
            },
            
            _ => {
                if c.is_ascii_digit() {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(LexerError::new(
                        &format!("Unexpected character: {}", c),
                        self.line,
                        self.column - 1
                    ));
                }
            }
        }
        
        Ok(())
    }
    
    fn line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn block_comment(&mut self) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_column = self.column - 2;
        let mut nesting = 1;
        
        while nesting > 0 {
            if self.is_at_end() {
                return Err(LexerError::new("Unterminated block comment", start_line, start_column));
            }
            
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance(); self.advance();
                nesting += 1;
            } else if self.peek() == '*' && self.peek_next() == '/' {
                self.advance(); self.advance();
                nesting -= 1;
            } else {
                if self.peek() == '\n' {
                    self.line += 1;
                    self.column = 0;
                }
                self.advance();
            }
        }
        
        Ok(())
    }

    /// Handles a forward slash character, which could be division, regexp, or comment
    fn handle_slash(&mut self) -> Result<(), LexerError> {
        if self.match_char('/') {
            self.line_comment();
        } else if self.match_char('*') {
            self.block_comment()?;
        } else if self.match_char('=') {
            add_token!(self, TokenType::SlashEqual, 2);
        } else if self.is_regexp_start() {
            self.regexp()?;
        } else {
            add_token!(self, TokenType::Slash);
        }
        Ok(())
    }

    /// Determines if a forward slash should be interpreted as the start of a regular expression
    /// rather than a division operator based on JavaScript syntax rules.
    fn is_regexp_start(&self) -> bool {
        if self.tokens.is_empty() {
            return true;
        }
                
        // Get the last token type
        let last_token = &self.tokens.last().unwrap().token_type;
                
        // A slash starts a regex if it follows a token that cannot be the end of an expression
        match last_token {

            // After these tokens, a slash is division (these can end an expression)
            TokenType::Identifier(_) | 
            TokenType::NumberLiteral(_) |
            TokenType::StringLiteral(_) |
            TokenType::RegExpLiteral(_, _) |
            TokenType::TemplateLiteral(_) |
            TokenType::True |
            TokenType::False |
            TokenType::Null |
            TokenType::This |
            TokenType::RightParen |
            TokenType::RightBracket |
            TokenType::PlusPlus |
            TokenType::MinusMinus => false,

            // Special case: right brace - could be block or object literal
            TokenType::RightBrace => {
                // TODO implement properly

                // This is a complex case that depends on context
                // For simplicity, we'll assume it's a block end, so division follows
                false
            },
            
            // After these tokens, a slash is always regex
            _ => true
        }
    }

    /// Parses a regular expression literal
    fn regexp(&mut self) -> Result<(), LexerError> {
        let start_column = self.column - 1;
        let mut pattern = String::with_capacity(16);
        
        // Parse the pattern
        while !self.is_at_end() && self.peek() != '/' {
            if self.peek() == '\\' {
                pattern.push(self.advance()); // Add the escape character
                
                if self.is_at_end() {
                    return Err(LexerError::new(
                        "Unterminated regular expression: escape sequence not completed",
                        self.line,
                        start_column
                    ));
                }
                
                // Add the escaped character (whatever it is)
                pattern.push(self.advance());
            } else if self.peek() == '\n' {
                return Err(LexerError::new(
                    "Unterminated regular expression: newline in pattern",
                    self.line,
                    start_column
                ));
            } else {
                pattern.push(self.advance());
            }
        }
        
        if self.is_at_end() {
            return Err(LexerError::new(
                "Unterminated regular expression",
                self.line,
                start_column
            ));
        }
        
        // Consume the closing slash
        self.advance();
        
        // Parse flags
        let mut flags = String::with_capacity(4);
        while !self.is_at_end() && self.is_regexp_flag(self.peek()) {
            flags.push(self.advance());
        }
        
        // Validate flags (no duplicates, only valid flags)
        let mut seen_flags = HashSet::with_capacity(flags.len());
        for flag in flags.chars() {
            if !seen_flags.insert(flag) {
                return Err(LexerError::new(
                    &format!("Duplicate flag '{}' in regular expression", flag),
                    self.line,
                    self.column - 1
                ));
            }
            
            if !matches!(flag, 'g' | 'i' | 'm' | 's' | 'u' | 'y' | 'd') {
                return Err(LexerError::new(
                    &format!("Invalid regular expression flag '{}'", flag),
                    self.line,
                    self.column - 1
                ));
            }
        }
        
        let length = (self.current - self.start) as usize;

        add_token!(self, TokenType::RegExpLiteral(pattern, flags), length);

        Ok(())
    }


    
    #[inline]
    fn is_regexp_flag(&self, c: char) -> bool {
        matches!(c, 'g' | 'i' | 'm' | 's' | 'u' | 'y' | 'd')
    }

    fn template_literal(&mut self) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        
        let mut parts = Vec::new();
        let mut current_text = String::with_capacity(32);
        
        while !self.is_at_end() && self.peek() != '`' {
            if self.peek() == '\n' {
                current_text.push('\n');
                self.line += 1;
                self.column = 0;
                self.advance();
                continue;
            }
            
            // Check for template expression: ${...}
            if self.peek() == '$' && self.peek_next() == '{' {
                // Add the current text as a part
                if !current_text.is_empty() {
                    parts.push(TemplatePart::String(current_text));
                    current_text = String::with_capacity(32);
                }
                
                // Consume ${ characters
                self.advance(); // $
                self.advance(); // {
                
                // Keep track of brace nesting
                let mut brace_count = 1;
                let expr_start = self.current;
                
                // Find the closing }
                while brace_count > 0 && !self.is_at_end() {
                    let c = self.peek();
                    if c == '{' {
                        brace_count += 1;
                    } else if c == '}' {
                        brace_count -= 1;
                    } else if c == '\n' {
                        self.line += 1;
                        self.column = 0;
                    }
                    
                    if brace_count > 0 {
                        self.advance();
                    }
                }
                
                if self.is_at_end() {
                    return Err(LexerError::new(
                        "Unterminated template expression",
                        start_line,
                        start_column
                    ));
                }
                
                // Extract the expression
                let expr = self.source[expr_start..self.current].to_string();
                parts.push(TemplatePart::Expression(expr));
                
                // Consume the closing }
                self.advance();
            } else if self.peek() == '\\' {
                // Handle escape sequences
                self.advance(); // Consume the backslash
                
                if self.is_at_end() {
                    return Err(LexerError::new(
                        "Unterminated template literal",
                        start_line,
                        start_column
                    ));
                }
                
                match self.peek() {
                    '\\' => current_text.push('\\'),
                    'n' => current_text.push('\n'),
                    'r' => current_text.push('\r'),
                    't' => current_text.push('\t'),
                    'b' => current_text.push('\u{0008}'), // backspace
                    'f' => current_text.push('\u{000C}'), // form feed
                    'v' => current_text.push('\u{000B}'), // vertical tab
                    '0' => current_text.push('\0'),
                    '\'' => current_text.push('\''),
                    '"' => current_text.push('"'),
                    '`' => current_text.push('`'),
                    '$' => current_text.push('$'),
                    '\n' => {
                        // Line continuation
                        self.line += 1;
                        self.column = 0;
                    },
                    'u' => {
                        // Unicode escape sequence
                        self.advance(); // Consume 'u'
                        current_text.push(self.parse_unicode_escape(start_line, start_column)?);
                        continue;
                    },
                    'x' => {
                        // Hexadecimal escape sequence
                        self.advance(); // Consume 'x'
                        current_text.push(self.parse_hex_escape(start_line, start_column)?);
                        continue;
                    },
                    _ => current_text.push(self.peek()),
                }
                
                self.advance();
            } else {
                current_text.push(self.advance());
            }
        }
        
        if self.is_at_end() {
            return Err(LexerError::new(
                "Unterminated template literal",
                start_line,
                start_column
            ));
        }
        
        // Add any remaining text
        if !current_text.is_empty() {
            parts.push(TemplatePart::String(current_text));
        }
        
        // Consume the closing backtick
        self.advance();
        
        let length = (self.current - self.start) as usize;
        
        add_token!(self, TokenType::TemplateLiteral(parts), length);
        
        Ok(())
    }
    
    fn string(&mut self, quote: char) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        
        let mut value = String::with_capacity(32);

        while !self.is_at_end() && self.peek() != quote {

            if self.peek() == '\n' {
                return Err(LexerError::new(
                    "Unterminated string literal",
                    start_line,
                    start_column
                ));
            }
            
            if self.peek() == '\\' {
                self.advance(); // Consume the backslash
                
                if self.is_at_end() {
                    return Err(LexerError::new(
                        "Unterminated string literal",
                        start_line,
                        start_column
                    ));
                }
                
                match self.peek() {
                    '\\' => value.push('\\'),
                    'n' => value.push('\n'),
                    'r' => value.push('\r'),
                    't' => value.push('\t'),
                    'b' => value.push('\u{0008}'), // backspace
                    'f' => value.push('\u{000C}'), // form feed
                    'v' => value.push('\u{000B}'), // vertical tab
                    '0' => value.push('\0'),
                    '\'' => value.push('\''),
                    '"' => value.push('"'),
                    '`' => value.push('`'),
                    '\n' => {
                        // Line continuation
                        self.line += 1;
                        self.column = 0;
                    },
                    'u' => {
                        // Unicode escape sequence
                        self.advance(); // Consume 'u'
                        value.push(self.parse_unicode_escape(start_line, start_column)?);
                        continue;
                    },
                    'x' => {
                        // Hexadecimal escape sequence
                        self.advance(); // Consume 'x'
                        value.push(self.parse_hex_escape(start_line, start_column)?);
                        continue;
                    },
                    _ => value.push(self.peek()),
                }
                
                self.advance();
            } else {
                value.push(self.advance());
            }
        }
        
        if self.is_at_end() {
            return Err(LexerError::new(
                "Unterminated string literal",
                start_line,
                start_column
            ));
        }
        
        // Consume the closing quote
        self.advance();        

        
        let length = (self.current - self.start) as usize;

        add_token!(self, TokenType::StringLiteral(value), length);

        Ok(())
    }

    fn parse_unicode_escape(&mut self, start_line: usize, start_column: usize) -> Result<char, LexerError> {
        if self.peek() == '{' {
            // Unicode code point escape \u{XXXXXX}
            self.advance(); // Consume '{'
            
            let hex_start = self.current;
            while !self.is_at_end() && self.peek() != '}' {
                if !self.is_hex_digit(self.peek()) {
                    return Err(LexerError::new(
                        "Invalid Unicode escape sequence: expected hex digits",
                        self.line,
                        self.column
                    ));
                }
                self.advance();
            }
            if self.is_at_end() || self.peek() != '}' {
                return Err(LexerError::new(
                    "Unterminated Unicode escape sequence",
                    start_line,
                    start_column
                ));
            }
            
            let hex_string = &self.source[hex_start..self.current];
            self.advance(); // Consume '}'
            
            if hex_string.is_empty() {
                return Err(LexerError::new(
                    "Empty Unicode code point escape sequence",
                    start_line,
                    start_column
                ));
            }
            
            match u32::from_str_radix(hex_string, 16) {
                Ok(code_point) => {
                    match std::char::from_u32(code_point) {
                        Some(c) => Ok(c),
                        None => Err(LexerError::new(
                            &format!("Invalid Unicode code point: {}", hex_string),
                            start_line,
                            start_column
                        ))
                    }
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid Unicode escape sequence: \\u{{{}}}", hex_string),
                    start_line,
                    start_column
                ))
            }
        } else {
            // Fixed 4-digit Unicode escape \uXXXX
            let mut hex_string = String::with_capacity(4);
            
            for _ in 0..4 {
                if self.is_at_end() || !self.is_hex_digit(self.peek()) {
                    return Err(LexerError::new(
                        "Invalid Unicode escape sequence: expected 4 hex digits",
                        start_line,
                        start_column
                    ));
                }
                hex_string.push(self.advance());
            }
            
            match u16::from_str_radix(&hex_string, 16) {
                Ok(code_unit) => {
                    match std::char::from_u32(code_unit as u32) {
                        Some(c) => Ok(c),
                        None => Err(LexerError::new(
                            &format!("Invalid Unicode code unit: {}", hex_string),
                            start_line,
                            start_column
                        ))
                    }
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid Unicode escape sequence: \\u{}", hex_string),
                    start_line,
                    start_column
                ))
            }
        }
    }

    fn parse_hex_escape(&mut self, start_line: usize, start_column: usize) -> Result<char, LexerError> {
        // Hexadecimal escape sequence \xXX
        let mut hex_string = String::with_capacity(2);
        
        for _ in 0..2 {
            if self.is_at_end() || !self.is_hex_digit(self.peek()) {
                return Err(LexerError::new(
                    "Invalid hex escape sequence: expected 2 hex digits",
                    start_line,
                    start_column
                ));
            }
            hex_string.push(self.advance());
        }
        
        match u8::from_str_radix(&hex_string, 16) {
            Ok(byte) => Ok(byte as char),
            Err(_) => Err(LexerError::new(
                &format!("Invalid hex escape sequence: \\x{}", hex_string),
                start_line,
                start_column
            ))
        }
    }

    fn number(&mut self) -> Result<(), LexerError> {
        let start_column = self.column - 1;
        
        // Check for binary, octal, or hex literals
        if self.peek_previous() == '0' {
            match self.peek() {
                'b' | 'B' => {
                    self.advance(); // Consume 'b' or 'B'
                    return self.binary_number(start_column);
                },
                'o' | 'O' => {
                    self.advance(); // Consume 'o' or 'O'
                    return self.octal_number(start_column);
                },
                'x' | 'X' => {
                    self.advance(); // Consume 'x' or 'X'
                    return self.hex_number(start_column);
                },
                _ => {}
            }
        }
        
        // Parse decimal part
        self.consume_digits();
        
        // Look for a decimal point
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance(); // Consume '.'
            self.consume_digits();
        }
        
        // Handle scientific notation
        if self.peek() == 'e' || self.peek() == 'E' {
            self.advance();
            
            // Optional sign
            if self.peek() == '+' || self.peek() == '-' {
                self.advance();
            }
            
            // Must have at least one digit
            if !self.is_digit(self.peek()) {
                return Err(LexerError::new(
                    "Invalid scientific notation: expected digit after exponent",
                    self.line,
                    self.column
                ));
            }
            
            self.consume_digits();
        }
        
        // Handle BigInt literals (123n)
        if self.peek() == 'n' {
            self.advance();
            
            // Extract the value without the 'n' suffix
            let value_str = self.extract_number_value(self.start, self.current - 1);
            
            // Validate BigInt (no decimal point, no exponent)
            if value_str.contains('.') || value_str.contains('e') || value_str.contains('E') {
                return Err(LexerError::new(
                    "BigInt literals cannot have decimal points or exponents",
                    self.line,
                    start_column
                ));
            }
            
            let length = (self.current - self.start) as usize;

            add_token!(self, TokenType::BigIntLiteral(value_str), length);

            return Ok(());
        }
        
        // Extract the value and parse it
        let value_str = self.extract_number_value(self.start, self.current);
        
        // Fast path for common integer cases
        if !value_str.contains('.') && !value_str.contains('e') && 
           !value_str.contains('E') && value_str.len() < 10 {
            // For small integers, parse directly to avoid floating point conversion
            if let Ok(int_val) = value_str.parse::<i32>() {
                let length = (self.current - self.start) as usize;
                
                add_token!(self, TokenType::NumberLiteral(int_val as f64), length);

                return Ok(());
            }
        }
        
        match value_str.parse::<f64>() {
            Ok(value) => {
                let length = (self.current - self.start) as usize;
                add_token!(self, TokenType::NumberLiteral(value), length);
                Ok(())
            },
            Err(_) => Err(LexerError::new(
                &format!("Invalid number: {}", value_str),
                self.line,
                start_column
            ))
        }
    }

    fn binary_number(&mut self, start_column: usize) -> Result<(), LexerError> {
        let start = self.current;
        
        // Consume binary digits
        while self.peek() == '0' || self.peek() == '1' || self.peek() == '_' {
            self.advance();
        }
        
        // Check if we have at least one binary digit
        if self.current == start {
            return Err(LexerError::new(
                "Invalid binary literal: expected at least one binary digit",
                self.line,
                start_column
            ));
        }
        
        // Handle BigInt literals (0b101n)
        if self.peek() == 'n' {
            self.advance();
            
            // Extract the value without the '0b' prefix and 'n' suffix
            let value_str = self.extract_number_value(start, self.current - 1);
            
            // Parse as binary
            match i64::from_str_radix(&value_str.replace('_', ""), 2) {
                Ok(_) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::BigIntLiteral(format!("0b{}", value_str)), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid binary BigInt literal: 0b{}", value_str),
                    self.line,
                    start_column
                ))
            }
        } else {
            // Extract the value without the '0b' prefix
            let value_str = self.extract_number_value(start, self.current);
            
            // Parse as binary and convert to f64
            match i64::from_str_radix(&value_str.replace('_', ""), 2) {
                Ok(value) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::NumberLiteral(value as f64), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid binary literal: 0b{}", value_str),
                    self.line,
                    start_column
                ))
            }
        }
    }

    fn octal_number(&mut self, start_column: usize) -> Result<(), LexerError> {
        let start = self.current;
        
        // Consume octal digits
        while self.is_octal_digit(self.peek()) || self.peek() == '_' {
            self.advance();
        }
        
        // Check if we have at least one octal digit
        if self.current == start {
            return Err(LexerError::new(
                "Invalid octal literal: expected at least one octal digit",
                self.line,
                start_column
            ));
        }
        
        // Handle BigInt literals (0o777n)
        if self.peek() == 'n' {
            self.advance();
            
            // Extract the value without the '0o' prefix and 'n' suffix
            let value_str = self.extract_number_value(start, self.current - 1);
            
            // Parse as octal
            match i64::from_str_radix(&value_str.replace('_', ""), 8) {
                Ok(_) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::BigIntLiteral(format!("0o{}", value_str)), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid octal BigInt literal: 0o{}", value_str),
                    self.line,
                    start_column
                ))
            }
        } else {
            // Extract the value without the '0o' prefix
            let value_str = self.extract_number_value(start, self.current);
            
            // Parse as octal and convert to f64
            match i64::from_str_radix(&value_str.replace('_', ""), 8) {
                Ok(value) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::NumberLiteral(value as f64), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid octal literal: 0o{}", value_str),
                    self.line,
                    start_column
                ))
            }
        }
    }


    fn hex_number(&mut self, start_column: usize) -> Result<(), LexerError> {
        let start = self.current;
        
        // Consume hex digits
        while self.is_hex_digit(self.peek()) || self.peek() == '_' {
            self.advance();
        }
        
        // Check if we have at least one hex digit
        if self.current == start {
            return Err(LexerError::new(
                "Invalid hexadecimal literal: expected at least one hex digit",
                self.line,
                start_column
            ));
        }
        
        // Handle BigInt literals (0xFFn)
        if self.peek() == 'n' {
            self.advance();
            
            // Extract the value without the '0x' prefix and 'n' suffix
            let value_str = self.extract_number_value(start, self.current - 1);
            
            // Parse as hex
            match i64::from_str_radix(&value_str.replace('_', ""), 16) {
                Ok(_) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::BigIntLiteral(format!("0x{}", value_str)), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid hexadecimal BigInt literal: 0x{}", value_str),
                    self.line,
                    start_column
                ))
            }
        } else {
            // Extract the value without the '0x' prefix
            let value_str = self.extract_number_value(start, self.current);
            
            // Parse as hex and convert to f64
            match i64::from_str_radix(&value_str.replace('_', ""), 16) {
                Ok(value) => {
                    let length = (self.current - self.start) as usize;
                    add_token!(self, TokenType::NumberLiteral(value as f64), length);
                    Ok(())
                },
                Err(_) => Err(LexerError::new(
                    &format!("Invalid hexadecimal literal: 0x{}", value_str),
                    self.line,
                    start_column
                ))
            }
        }
    }

    #[inline]
    fn consume_digits(&mut self) {
        while self.is_digit(self.peek()) || self.peek() == '_' {
            self.advance();
        }
    }
    
    fn extract_number_value(&self, start: usize, end: usize) -> String {
        // Remove numeric separators (_)
        let mut value_str = String::with_capacity(end - start);
        for c in self.source[start..end].chars() {
            if c != '_' {
                value_str.push(c);
            }
        }
        value_str
    }

    fn identifier(&mut self) {
        let start_column = self.column - 1;
        
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }
        
        // Get the identifier text
        let text = &self.source[self.start..self.current];
        
        // Check if it's a keyword using a match statement for better performance
        let token_type = match text {
            "break" => TokenType::Break,
            "case" => TokenType::Case,
            "catch" => TokenType::Catch,
            "class" => TokenType::Class,
            "const" => TokenType::Const,
            "continue" => TokenType::Continue,
            "debugger" => TokenType::Debugger,
            "default" => TokenType::Default,
            "delete" => TokenType::Delete,
            "do" => TokenType::Do,
            "else" => TokenType::Else,
            "enum" => TokenType::Enum,
            "export" => TokenType::Export,
            "extends" => TokenType::Extends,
            "false" => TokenType::False,
            "finally" => TokenType::Finally,
            "for" => TokenType::For,
            "function" => TokenType::Function,
            "if" => TokenType::If,
            "import" => TokenType::Import,
            "in" => TokenType::In,
            "instanceof" => TokenType::InstanceOf,
            "new" => TokenType::New,
            "null" => TokenType::Null,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "undefined" => TokenType::Undefined,
            "constructor" => TokenType::Constructor,
            "switch" => TokenType::Switch,
            "this" => TokenType::This,
            "throw" => TokenType::Throw,
            "true" => TokenType::True,
            "try" => TokenType::Try,
            "typeof" => TokenType::Typeof,
            "var" => TokenType::Var,
            "void" => TokenType::Void,
            "while" => TokenType::While,
            "with" => TokenType::With,
            "yield" => TokenType::Yield,
            "async" => TokenType::Async,
            "await" => TokenType::Await,
            "let" => TokenType::Let,
            "static" => TokenType::Static,
            "get" => TokenType::Get,
            "set" => TokenType::Set,
            "of" => TokenType::Of,
            "as" => TokenType::As,
            "from" => TokenType::From,
            "target" => TokenType::Target,
            "implements" => TokenType::Implements,
            "interface" => TokenType::Interface,
            "package" => TokenType::Package,
            "private" => TokenType::Private,
            "protected" => TokenType::Protected,
            "public" => TokenType::Public,
            "arguments" => TokenType::Arguments,
            "eval" => TokenType::Eval,
            _ => TokenType::Identifier(text.to_string()),
        };
        
        let length = (self.current - self.start) as usize;

        add_token!(self, token_type, length);
    }

    #[inline]
    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }
    
    #[inline]
    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_' || c == '$'
    }
    
    #[inline]
    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }
    
    #[inline]
    fn is_hex_digit(&self, c: char) -> bool {
        c.is_ascii_hexdigit()
    }
    
    #[inline]
    fn is_octal_digit(&self, c: char) -> bool {
        c >= '0' && c <= '7'
    }
    
    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }
    
    #[inline]
    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        self.column += 1;
        c
    }

    #[inline]
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }
    
    #[inline]
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }
    
    #[inline]
    fn peek_previous(&self) -> char {
        if self.current == 0 {
            '\0'
        } else {
            self.chars[self.current - 1]
        }
    }
    
    #[inline]
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            self.column += 1;
            true
        }
    }
    
}

