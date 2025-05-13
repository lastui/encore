use std::collections::HashSet;
use crate::lexer::{Token, TemplatePart, LexerError};

pub struct Lexer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    source_len: usize,
    tokens: Vec<(Token, [usize; 2])>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    current_char: char,
    previous_char: char,
}

macro_rules! emit_token {
    ($lexer:expr, $token:expr) => {
        $lexer.tokens.push(($token, [$lexer.line, $lexer.column]))
    }
}

impl<'a> Lexer<'a> {

    #[inline]
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            bytes: source.as_bytes(),
            source_len: source.len(),
            tokens: Vec::with_capacity(source.len() / 4),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            current_char: '\0',
            previous_char: '\0',
        }
    }

    #[inline(always)]
    fn identifier(&mut self) {        
        // Track whether the identifier is all ASCII
        let mut is_all_ascii = true;

        // Fast path for identifiers (most common case)
        while !self.is_at_end() {
            if self.current < self.source_len {
                let b = self.bytes[self.current];
                
                // Fast check for ASCII alphanumeric characters
                if (b >= b'a' && b <= b'z') || 
                   (b >= b'A' && b <= b'Z') || 
                   (b >= b'0' && b <= b'9') || 
                   b == b'_' || 
                   b == b'$' {
                    // Advance without the overhead of UTF-8 decoding
                    self.previous_char = self.current_char;
                    self.current_char = b as char;
                    self.current += 1;
                    self.column += 1;
                    continue;
                } else if b >= 128 {
                    // Found a non-ASCII byte
                    is_all_ascii = false;
                    // Process it with the regular advance method
                    self.advance();
                    continue;
                }
            }

            // If we reach here, either we're at the end or the next character 
            // is not an identifier character
            if !self.is_at_end() && self.is_alphanumeric(self.peek()) {
                let c = self.advance();
                // Check if we just processed a non-ASCII character
                if !c.is_ascii() {
                    is_all_ascii = false;
                }
            } else {
                break;
            }
        }

        // Calculate the length of the identifier
        let length = self.current - self.start;

        // Only check for keywords if the identifier is within the length range of keywords
        // and is all ASCII (since all keywords are ASCII)
        let token_type = if is_all_ascii && length >= 2 && length <= 10 {
            // For ASCII identifiers, we can do direct byte comparisons
            let bytes = &self.bytes[self.start..self.current];
            
            // First check by length for faster matching
            match bytes.len() {
                2 => match bytes {
                    b"do" => Token::Do,
                    b"if" => Token::If,
                    b"in" => Token::In,
                    b"of" => Token::Of,
                    b"as" => Token::As,
                    _ => self.create_identifier_token(),
                },
                3 => match bytes {
                    b"for" => Token::For,
                    b"let" => Token::Let,
                    b"new" => Token::New,
                    b"try" => Token::Try,
                    b"var" => Token::Var,
                    b"get" => Token::Get,
                    b"set" => Token::Set,
                    _ => self.create_identifier_token(),
                },
                4 => match bytes {
                    b"case" => Token::Case,
                    b"else" => Token::Else,
                    b"enum" => Token::Enum,
                    b"from" => Token::From,
                    b"null" => Token::Null,
                    b"this" => Token::This,
                    b"true" => Token::True,
                    b"void" => Token::Void,
                    b"with" => Token::With,
                    b"eval" => Token::Eval,
                    _ => self.create_identifier_token(),
                },
                5 => match bytes {
                    b"async" => Token::Async,
                    b"await" => Token::Await,
                    b"break" => Token::Break,
                    b"catch" => Token::Catch,
                    b"class" => Token::Class,
                    b"const" => Token::Const,
                    b"false" => Token::False,
                    b"super" => Token::Super,
                    b"throw" => Token::Throw,
                    b"while" => Token::While,
                    b"yield" => Token::Yield,
                    _ => self.create_identifier_token(),
                },
                6 => match bytes {
                    b"delete" => Token::Delete,
                    b"export" => Token::Export,
                    b"import" => Token::Import,
                    b"public" => Token::Public,
                    b"return" => Token::Return,
                    b"static" => Token::Static,
                    b"switch" => Token::Switch,
                    b"target" => Token::Target,
                    b"typeof" => Token::Typeof,
                    _ => self.create_identifier_token(),
                },
                7 => match bytes {
                    b"default" => Token::Default,
                    b"extends" => Token::Extends,
                    b"finally" => Token::Finally,
                    b"package" => Token::Package,
                    b"private" => Token::Private,
                    _ => self.create_identifier_token(),
                },
                8 => match bytes {
                    b"continue" => Token::Continue,
                    b"debugger" => Token::Debugger,
                    b"function" => Token::Function,
                    _ => self.create_identifier_token(),
                },
                9 => match bytes {
                    b"arguments" => Token::Arguments,
                    b"interface" => Token::Interface,
                    b"protected" => Token::Protected,
                    b"undefined" => Token::Undefined,
                    _ => self.create_identifier_token(),
                },
                10 => match bytes {
                    b"instanceof" => Token::InstanceOf,
                    b"implements" => Token::Implements,
                    b"constructor" => Token::Constructor,
                    _ => self.create_identifier_token(),
                },
                _ => self.create_identifier_token(),
            }
        } else {
            // For non-ASCII identifiers or identifiers with lengths outside keyword range
            self.create_identifier_token()
        };
        
        // Add the token
        emit_token!(self, token_type);
    }

    // Helper method to create an identifier token
    #[inline]
    fn create_identifier_token(&self) -> Token {
        let text = &self.source[self.start..self.current];
        Token::Identifier(text.to_string())
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<(Token, [usize; 2])>, LexerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        let _eof_column = self.column;
        emit_token!(self, Token::EOS);
        Ok(std::mem::take(&mut self.tokens))
    }
    
    fn scan_token(&mut self) -> Result<(), LexerError> {
        let c = self.advance();
        
        match c {
            '(' => emit_token!(self, Token::LeftParen),
            ')' => emit_token!(self, Token::RightParen),
            '{' => emit_token!(self, Token::LeftBrace),
            '}' => emit_token!(self, Token::RightBrace),
            '[' => emit_token!(self, Token::LeftBracket),
            ']' => emit_token!(self, Token::RightBracket),
            ',' => emit_token!(self, Token::Comma),
            ';' => emit_token!(self, Token::Semicolon),
            ':' => emit_token!(self, Token::Colon),
            '#' => emit_token!(self, Token::Hash),
            
            '.' => {
                if self.match_char('.') && self.match_char('.') {
                    emit_token!(self, Token::Ellipsis);
                } else {
                    emit_token!(self, Token::Dot);
                }
            },                
            '+' => {
                if self.match_char('+') {
                    emit_token!(self, Token::PlusPlus);
                } else if self.match_char('=') {
                    emit_token!(self, Token::PlusEqual);
                } else {
                    emit_token!(self, Token::Plus);
                }
            },
            '-' => {
                if self.match_char('-') {
                    emit_token!(self, Token::MinusMinus);
                } else if self.match_char('=') {
                    emit_token!(self, Token::MinusEqual);
                } else {
                    emit_token!(self, Token::Minus);
                }
            },
            '%' => {
                if self.match_char('=') {
                    emit_token!(self, Token::PercentEqual);
                } else {
                    emit_token!(self, Token::Percent);
                }
            },
            '^' => {
                if self.match_char('=') {
                    emit_token!(self, Token::CaretEqual);
                } else {
                    emit_token!(self, Token::Caret);
                }
            },
            '*' => {
                if self.match_char('*') {
                    if self.match_char('=') {
                        emit_token!(self, Token::StarStarEqual);
                    } else {
                        emit_token!(self, Token::StarStar);
                    }
                } else if self.match_char('=') {
                    emit_token!(self, Token::StarEqual);
                } else {
                    emit_token!(self, Token::Star);
                }
            },
            '/' => self.handle_slash()?,
            '!' => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        emit_token!(self, Token::BangEqualEqual);
                    } else {
                        emit_token!(self, Token::BangEqual);
                    }
                } else {
                    emit_token!(self, Token::Bang);
                }
            },
            '=' => {
                if self.match_char('>') {
                    emit_token!(self, Token::Arrow);
                } else if self.match_char('=') {
                    if self.match_char('=') {
                        emit_token!(self, Token::EqualEqualEqual);
                    } else {
                        emit_token!(self, Token::EqualEqual);
                    }
                } else {
                    emit_token!(self, Token::Equal);
                }
            },
            
            '<' => {
                if self.match_char('=') {
                    emit_token!(self, Token::LessEqual);
                } else if self.match_char('<') {
                    if self.match_char('=') {
                        emit_token!(self, Token::LessLessEqual);
                    } else {
                        emit_token!(self, Token::LessLess);
                    }
                } else {
                    emit_token!(self, Token::Less);
                }
            },
            
            '>' => {
                if self.match_char('=') {
                    emit_token!(self, Token::GreaterEqual);
                } else if self.match_char('>') {
                    if self.match_char('>') {
                        if self.match_char('=') {
                            emit_token!(self, Token::GreaterGreaterGreaterEqual);
                        } else {
                            emit_token!(self, Token::GreaterGreaterGreater);
                        }
                    } else if self.match_char('=') {
                        emit_token!(self, Token::GreaterGreaterEqual);
                    } else {
                        emit_token!(self, Token::GreaterGreater);
                    }
                } else {
                    emit_token!(self, Token::Greater);
                }
            },
            
            '&' => {
                if self.match_char('&') {
                    if self.match_char('=') {
                        emit_token!(self, Token::AmpersandAmpersandEqual);
                    } else {
                        emit_token!(self, Token::AmpersandAmpersand);
                    }
                } else if self.match_char('=') {
                    emit_token!(self, Token::AmpersandEqual);
                } else {
                    emit_token!(self, Token::Ampersand);
                }
            },
            
            '|' => {
                if self.match_char('|') {
                    if self.match_char('=') {
                        emit_token!(self, Token::PipePipeEqual);
                    } else {
                        emit_token!(self, Token::PipePipe);
                    }
                } else if self.match_char('=') {
                    emit_token!(self, Token::PipeEqual);
                } else {
                    emit_token!(self, Token::Pipe);
                }
            },
            '~' => emit_token!(self, Token::Tilde),            
            '?' => {
                if self.match_char('?') {
                    if self.match_char('=') {
                        emit_token!(self, Token::QuestionQuestionEqual);
                    } else {
                        emit_token!(self, Token::QuestionQuestion);
                    }
                } else if self.match_char('.') {
                    emit_token!(self, Token::QuestionDot);
                } else {
                    emit_token!(self, Token::Question);
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
    
    #[inline(always)]
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

    #[inline]
    fn handle_slash(&mut self) -> Result<(), LexerError> {
        if self.match_char('/') {
            self.line_comment();
        } else if self.match_char('*') {
            self.block_comment()?;
        } else if self.match_char('=') {
            emit_token!(self, Token::SlashEqual);
        } else if self.is_regexp_start() {
            self.regexp()?;
        } else {
            emit_token!(self, Token::Slash);
        }
        Ok(())
    }

    #[inline]
    fn is_regexp_start(&self) -> bool {
        if self.tokens.is_empty() {
            return true;
        }
                
        // Get the last token type
        let (last_token, _) = &self.tokens.last().unwrap();
                
        // A slash starts a regex if it follows a token that cannot be the end of an expression
        match last_token {

            // After these tokens, a slash is division (these can end an expression)
            Token::Identifier(_) | 
            Token::NumberLiteral(_) |
            Token::StringLiteral(_) |
            Token::RegExpLiteral(_, _) |
            Token::TemplateLiteral(_) |
            Token::True |
            Token::False |
            Token::Null |
            Token::This |
            Token::RightParen |
            Token::RightBracket |
            Token::PlusPlus |
            Token::MinusMinus => false,

            // Special case: right brace - could be block or object literal
            Token::RightBrace => {
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

        emit_token!(self, Token::RegExpLiteral(pattern, flags));

        Ok(())
    }

    #[inline(always)]
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
        
        emit_token!(self, Token::TemplateLiteral(parts));
        
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

        emit_token!(self, Token::StringLiteral(value));

        Ok(())
    }

    fn parse_unicode_escape(&mut self, start_line: usize, start_column: usize) -> Result<char, LexerError> {  
        if self.peek() == '{' {  
            // Unicode code point escape \u{XXXXXX} - this part is already correct  
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
                    // Check if this is a high surrogate  
                    if (0xD800..=0xDBFF).contains(&code_unit) {  
                        // This is a high surrogate, we need to look for a low surrogate  
                        if self.peek() == '\\' && self.peek_next() == 'u' {  
                            // Save current position in case we need to revert  
                            let save_current = self.current;  
                            let save_line = self.line;  
                            let save_column = self.column;  
                              
                            // Consume the \u  
                            self.advance(); // \  
                            self.advance(); // u  
                              
                            // Parse the next 4 hex digits  
                            let mut low_hex = String::with_capacity(4);  
                            let mut valid_low_surrogate = true;  
                              
                            for _ in 0..4 {  
                                if self.is_at_end() || !self.is_hex_digit(self.peek()) {  
                                    valid_low_surrogate = false;  
                                    break;  
                                }  
                                low_hex.push(self.advance());  
                            }  
                              
                            if valid_low_surrogate {  
                                if let Ok(low_code_unit) = u16::from_str_radix(&low_hex, 16) {  
                                    if (0xDC00..=0xDFFF).contains(&low_code_unit) {  
                                        // Valid surrogate pair, calculate the Unicode code point  
                                        let code_point = 0x10000 + ((code_unit - 0xD800) as u32 * 0x400) + (low_code_unit - 0xDC00) as u32;  
                                        return match std::char::from_u32(code_point) {  
                                            Some(c) => Ok(c),  
                                            None => Err(LexerError::new(  
                                                &format!("Invalid Unicode surrogate pair: \\u{}\\u{}", hex_string, low_hex),  
                                                start_line,  
                                                start_column  
                                            ))  
                                        };  
                                    }  
                                }  
                            }  
                              
                            // If we get here, the sequence after the high surrogate wasn't a valid low surrogate  
                            // Revert to the position after the high surrogate  
                            self.current = save_current;  
                            self.line = save_line;  
                            self.column = save_column;  
                        }  
                          
                        // Lone high surrogate without a following low surrogate  
                        // In strict mode, this should be an error, but JavaScript allows it  
                        // and replaces it with a replacement character  
                        return Ok('\u{FFFD}'); // Unicode replacement character  
                    }  
                      
                    // Check if this is a low surrogate without a preceding high surrogate  
                    if (0xDC00..=0xDFFF).contains(&code_unit) {  
                        // Lone low surrogate, also replace with replacement character  
                        return Ok('\u{FFFD}');  
                    }  
                      
                    // Regular BMP character  
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

    #[inline]
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

            emit_token!(self, Token::BigIntLiteral(value_str));

            return Ok(());
        }
        
        // Extract the value and parse it
        let value_str = self.extract_number_value(self.start, self.current);
        
        // Fast path for common integer cases
        if !value_str.contains('.') && !value_str.contains('e') && 
           !value_str.contains('E') && value_str.len() < 10 {
            // For small integers, parse directly to avoid floating point conversion
            if let Ok(int_val) = value_str.parse::<i32>() {
                emit_token!(self, Token::NumberLiteral(int_val as f64));
                return Ok(());
            }
        }
        
        match value_str.parse::<f64>() {
            Ok(value) => {
                emit_token!(self, Token::NumberLiteral(value));
                Ok(())
            },
            Err(_) => Err(LexerError::new(
                &format!("Invalid number: {}", value_str),
                self.line,
                start_column
            ))
        }
    }

    #[inline]
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
                    emit_token!(self, Token::BigIntLiteral(format!("0b{}", value_str)));
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
                    emit_token!(self, Token::NumberLiteral(value as f64));
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

    #[inline]
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
                    emit_token!(self, Token::BigIntLiteral(format!("0o{}", value_str)));
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
                    emit_token!(self, Token::NumberLiteral(value as f64));
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

    #[inline]
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
                    emit_token!(self, Token::BigIntLiteral(format!("0x{}", value_str)));
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
                    emit_token!(self, Token::NumberLiteral(value as f64));
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

    #[inline(always)]
    fn consume_digits(&mut self) {
        while self.is_digit(self.peek()) || self.peek() == '_' {
            self.advance();
        }
    }

    #[inline]
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

    #[inline(always)]
    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'  // Direct comparison is faster than is_ascii_digit()
    }
    
    #[inline(always)]
    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'
    }
    
    #[inline(always)]
    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }
    
    #[inline(always)]
    fn is_hex_digit(&self, c: char) -> bool {
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    }
    
    #[inline(always)]
    fn is_at_end(&self) -> bool {
        self.current >= self.source_len
    }

    #[inline(always)]
    fn is_octal_digit(&self, c: char) -> bool {
        c >= '0' && c <= '7'
    }
    
    #[inline(always)]
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        // Fast path for ASCII (most common case in JS)
        if self.current < self.source_len && self.bytes[self.current] < 128 {
            let c = self.bytes[self.current] as char;
            self.previous_char = self.current_char;
            self.current_char = c;
            self.current += 1;
            self.column += 1;
            return c;
        }

        // Fallback for non-ASCII (UTF-8)
        let c = self.source[self.current..].chars().next().unwrap();
        self.previous_char = self.current_char;
        self.current_char = c;
        self.current += c.len_utf8();
        self.column += 1;
        c
    }

    #[inline(always)]
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        if self.bytes[self.current] < 128 {
            return self.bytes[self.current] as char;
        }
        self.source[self.current..].chars().next().unwrap()
    }

    #[inline(always)]
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source_len {
            return '\0';
        }
        // Fast path for ASCII
        if self.bytes[self.current] < 128 && self.bytes[self.current + 1] < 128 {
            return self.bytes[self.current + 1] as char;
        }
        // If current is ASCII but next might not be
        if self.bytes[self.current] < 128 {
            let next_pos = self.current + 1;
            return self.source[next_pos..].chars().next().unwrap_or('\0');
        }
        // Both current and next are non-ASCII
        let mut iter = self.source[self.current..].chars();
        iter.next();
        iter.next().unwrap_or('\0')
    }

    #[inline(always)]
    fn peek_previous(&self) -> char {
        self.previous_char
    }

    #[inline(always)]
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }
}
