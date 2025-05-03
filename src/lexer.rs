use crate::ast::*;

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub line: usize,
}

impl LexerError {
    pub fn new(message: &str, line: usize) -> Self {
        LexerError {
            message: message.to_string(),
            line,
        }
    }
}

impl From<LexerError> for String {
    fn from(error: LexerError) -> Self {
        error.message.clone()
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO improve error reporting show original source code fragment and arrow where the error is
        write!(f, "LexerError at line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for LexerError {}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Question,
    Arrow,
    // One or two character tokens
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Star,
    StarStar,
    StarEqual,
    StarStarEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Bang,
    BangEqual,
    BangEqualEqual,
    Greater,
    GreaterEqual,
    GreaterGreater,
    GreaterGreaterEqual,
    GreaterGreaterGreater,
    GreaterGreaterGreaterEqual,
    Less,
    LessEqual,
    LessLess,
    LessLessEqual,
    Ampersand,
    AmpersandAmpersand,
    AmpersandEqual,
    Pipe,
    PipePipe,
    PipeEqual,
    Caret,
    CaretEqual,
    Tilde,
    Ellipsis,
    QuestionQuestion,
    // Literals
    Identifier(String),
    StringLiteral(String),
    TemplateLiteral(Vec<TemplatePart>),
    NumberLiteral(f64),
    // TODO Symbol
    True,
    False,
    Null,
    Undefined,
    //BooleanLiteral(bool),
    //NullLiteral,
    //UndefinedLiteral,
    // Keywords
    Var,
    Let,
    Const,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    And,
    Or,
    TypeOf,
    Break,
    Continue,
    This,
    New,
    Delete,
    Typeof,
    Void,
    In,
    InstanceOf,
    Try,
    Catch,
    Finally,
    Throw,
    Switch,
    Case,
    Default,
    Await,
    Async,
    // ES Module keywords
    Import,
    Export,
    From,
    As,
    // End of file
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    String(String),
    Expression(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize) -> Self {
        Token { token_type, line }
    }
}

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.to_string(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token::new(TokenType::EOF, self.line));
        Ok(self.tokens.clone())
    }
    
    fn scan_token(&mut self) -> Result<(), LexerError> {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),

            // TODO missing elvis operator
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::Semicolon),
            ':' => self.add_token(TokenType::Colon),
            // Operators
            '+' => {
                if self.match_char('+') {
                    self.add_token(TokenType::PlusPlus);
                } else if self.match_char('=') {
                    self.add_token(TokenType::PlusEqual);
                } else {
                    self.add_token(TokenType::Plus);
                }
            },
            '-' => {
                if self.match_char('-') {
                    self.add_token(TokenType::MinusMinus);
                } else if self.match_char('=') {
                    self.add_token(TokenType::MinusEqual);
                } else {
                    self.add_token(TokenType::Minus);
                }
            },
            '*' => {
                if self.match_char('*') {
                    if self.match_char('=') {
                        self.add_token(TokenType::StarStarEqual);
                    } else {
                        self.add_token(TokenType::StarStar);
                    }
                } else if self.match_char('=') {
                    self.add_token(TokenType::StarEqual);
                } else {
                    self.add_token(TokenType::Star);
                }
            },
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    self.block_comment()?;
                } else if self.match_char('=') {
                    self.add_token(TokenType::SlashEqual);
                } else {
                    self.add_token(TokenType::Slash);
                }
            },
            '%' => {
                if self.match_char('=') {
                    self.add_token(TokenType::PercentEqual);
                } else {
                    self.add_token(TokenType::Percent);
                }
            },
            '!' => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        self.add_token(TokenType::BangEqualEqual);
                    } else {
                        self.add_token(TokenType::BangEqual);
                    }
                } else {
                    self.add_token(TokenType::Bang);
                }
            },
            '=' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Arrow);
                } else if self.match_char('=') {
                    if self.match_char('=') {
                        self.add_token(TokenType::EqualEqualEqual);
                    } else {
                        self.add_token(TokenType::EqualEqual);
                    }
                } else {
                    self.add_token(TokenType::Equal);
                }
            },
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else if self.match_char('<') {
                    if self.match_char('=') {
                        self.add_token(TokenType::LessLessEqual);
                    } else {
                        self.add_token(TokenType::LessLess);
                    }
                } else {
                    self.add_token(TokenType::Less);
                }
            },
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else if self.match_char('>') {
                    if self.match_char('>') {
                        if self.match_char('=') {
                            self.add_token(TokenType::GreaterGreaterGreaterEqual);
                        } else {
                            self.add_token(TokenType::GreaterGreaterGreater);
                        }
                    } else if self.match_char('=') {
                        self.add_token(TokenType::GreaterGreaterEqual);
                    } else {
                        self.add_token(TokenType::GreaterGreater);
                    }
                } else {
                    self.add_token(TokenType::Greater);
                }
            },
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::AmpersandAmpersand);
                } else if self.match_char('=') {
                    self.add_token(TokenType::AmpersandEqual);
                } else {
                    self.add_token(TokenType::Ampersand);
                }
            },
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::PipePipe);
                } else if self.match_char('=') {
                    self.add_token(TokenType::PipeEqual);
                } else {
                    self.add_token(TokenType::Pipe);
                }
            },
            '^' => {
                if self.match_char('=') {
                    self.add_token(TokenType::CaretEqual);
                } else {
                    self.add_token(TokenType::Caret);
                }
            },
            '~' => self.add_token(TokenType::Tilde),
            '?' => {
                if self.match_char('?') {
                    self.add_token(TokenType::QuestionQuestion);
                } else {
                    self.add_token(TokenType::Question);
                }
            },

            // String literals
            '"' => self.string('"')?,
            '\'' => self.string('\'')?,
            '`' => self.template_literal()?,

            // Whitespace
            ' ' | '\r' | '\t' => {
                // Ignore whitespace
            },
            '\n' => {
                self.line += 1;
            },
            
            // Default case
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(LexerError::new(&format!("Unexpected character: {}", c), self.line));
                }
            }
        }
        
        Ok(())
    }

    fn template_literal(&mut self) -> Result<(), LexerError> {
        // We've already consumed the opening backtick
        
        let mut parts = Vec::new();
        let mut current_text = String::new();
        
        while self.peek() != '`' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            
            // Check for template expression: ${...}
            if self.peek() == '$' && self.peek_next() == '{' {
                // Add the current text as a part
                if !current_text.is_empty() {
                    parts.push(TemplatePart::String(current_text));
                    current_text = String::new();
                }
                
                // Consume ${ characters
                self.advance(); // $
                self.advance(); // {
                
                // Keep track of brace nesting
                let mut brace_count = 1;
                let expr_start = self.current;
                
                // Find the closing }
                while brace_count > 0 && !self.is_at_end() {
                    if self.peek() == '{' {
                        brace_count += 1;
                    } else if self.peek() == '}' {
                        brace_count -= 1;
                    } else if self.peek() == '\n' {
                        self.line += 1;
                    }
                    
                    if brace_count > 0 {
                        self.advance();
                    }
                }
                
                if self.is_at_end() {
                    return Err(LexerError::new("Unterminated template expression", self.line));
                }
                
                // Extract the expression
                let expr = self.source[expr_start..self.current].to_string();
                parts.push(TemplatePart::Expression(expr));
                
                // Consume the closing }
                self.advance();
            } else if self.peek() == '\\' && (self.peek_next() == '`' || self.peek_next() == '$') {
                // Handle escape sequences for backticks and dollar signs
                self.advance(); // Consume the escape character
                current_text.push(self.advance());
            } else {
                current_text.push(self.advance());
            }
        }
        
        if self.is_at_end() {
            return Err(LexerError::new("Unterminated template literal", self.line));
        }
        
        // Add any remaining text
        if !current_text.is_empty() {
            parts.push(TemplatePart::String(current_text));
        }
        
        // Consume the closing backtick
        self.advance();
        
        // Add the template literal token
        self.add_token(TokenType::TemplateLiteral(parts));
        
        Ok(())
    }

    fn block_comment(&mut self) -> Result<(), LexerError> {
        // We've already consumed /* at this point
        let mut nesting = 1;
        
        while nesting > 0 {
            if self.is_at_end() {
                return Err(LexerError::new("Unterminated block comment", self.line));
            }
            
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance(); // Consume /
                self.advance(); // Consume *
                nesting += 1;
            } else if self.peek() == '*' && self.peek_next() == '/' {
                self.advance(); // Consume *
                self.advance(); // Consume /
                nesting -= 1;
            } else {
                if self.peek() == '\n' {
                    self.line += 1;
                }
                self.advance();
            }
        }

        Ok(())
    }
    
    fn string(&mut self, quote: char) -> Result<(), LexerError> {
        // We've already consumed the opening quote
        
        while self.peek() != quote && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            
            if self.peek() == '\\' && self.peek_next() == quote {
                self.advance(); // Consume the escape character
            }
            
            self.advance();
        }
        
        if self.is_at_end() {
            return Err(LexerError::new("Unterminated string", self.line));
        }
        
        // Consume the closing quote
        self.advance();
        
        // Extract the string value (without the quotes)
        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token(TokenType::StringLiteral(value));
        
        Ok(())
    }
    
    fn number(&mut self) -> Result<(), LexerError> {
        while self.is_digit(self.peek()) {
            self.advance();
        }
        
        // TODO sci notation

        // TODO python 1_000_000 notation

        // Look for a decimal part
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // Consume the "."
            self.advance();
            
            while self.is_digit(self.peek()) {
                self.advance();
            }
        }
        
        // Parse the number
        let value = match self.source[self.start..self.current].parse::<f64>() {
            Ok(num) => num,
            Err(_) => return Err(LexerError::new("Invalid number", self.line)),
        };
        
        self.add_token(TokenType::NumberLiteral(value));
        Ok(())
    }
    
    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }
        
        // See if the identifier is a reserved word
        let text = &self.source[self.start..self.current];
        
        let token_type = match text {
            // todo label
            // todo goto
            "var" => TokenType::Var,
            "let" => TokenType::Let,
            "const" => TokenType::Const,
            "function" => TokenType::Function,
            // todo function*
            "return" => TokenType::Return,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            // TODO do
            "while" => TokenType::While,
            "for" => TokenType::For,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "null" => TokenType::Null,
            "undefined" => TokenType::Undefined,
            "void 0" => TokenType::Undefined,
            "this" => TokenType::This,
            "new" => TokenType::New,
            "delete" => TokenType::Delete,
            "typeof" => TokenType::Typeof,
            "void" => TokenType::Void,
            "in" => TokenType::In,
            "instanceof" => TokenType::InstanceOf,
            "try" => TokenType::Try,
            "catch" => TokenType::Catch,
            "finally" => TokenType::Finally,
            "throw" => TokenType::Throw,
            "switch" => TokenType::Switch,
            "case" => TokenType::Case,
            "default" => TokenType::Default,
            "await" => TokenType::Await,
            "async" => TokenType::Async,
            // ES Module keywords
            "import" => TokenType::Import,
            "export" => TokenType::Export,
            "from" => TokenType::From,
            "as" => TokenType::As,
            // TODO CJS Module Keywords?
            _ => TokenType::Identifier(text.to_string()),
        };
        
        self.add_token(token_type);
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap_or('\0');
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap_or('\0') != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap_or('\0')
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'
    }
    
    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }
    
    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }
    
    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(token_type, self.line));
    }
}
