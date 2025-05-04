use crate::ast::*;
use crate::lexer::{Token, TokenType};
use super::error::{ParserError, ParseResult};
use super::state::ParserState;
use std::collections::HashSet;


pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub comments: Vec<Comment>,
    pub state: ParserState,
    pub source: Option<String>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
            comments: Vec::new(),
            state: ParserState::new(),
            source: None,
        }
    }


    // Method to attach source code to an existing parser
    pub fn attach_source(&mut self, source: &str) {
        self.source = Some(source.to_string());
    }

    // Token navigation methods
    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || matches!(self.peek_token_type(), Some(TokenType::EOF))
    }

    pub fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn peek_token_type(&self) -> Option<&TokenType> {
        self.peek_token().map(|t| &t.token_type)
    }

    pub fn previous(&self) -> Option<&Token> {
        if self.current > 0 {
            self.tokens.get(self.current - 1)
        } else {
            None
        }
    }

    pub fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        match self.peek_token_type() {
            Some(t) => t == token_type,
            None => false,
        }
    }

    pub fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_any(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn consume(&mut self, token_type: &TokenType, message: &str) -> ParseResult<Token> {
        thread_local! {
            static DUMMY_TOKEN: Token = Token::new(TokenType::EOF, 0, 0, 0);
        }

        if token_type == &TokenType::Semicolon {
            if self.match_token(&TokenType::Semicolon) {
                return Ok(self.previous().unwrap().clone());
            }

            // ASI rules: insert semicolon if
            // 1. The current token is on a new line from the previous token
            // 2. The current token is a closing brace
            // 3. We've reached the end of input
            if self.previous_line_terminator() || 
               self.check(&TokenType::RightBrace) || 
               self.is_at_end() {
                if let Some(prev) = self.previous() {
                    return Ok(prev.clone());
                } else {
                    return DUMMY_TOKEN.with(|token| Ok(token.clone()));
                }
            }
        } else if self.check(token_type) {
            return Ok(self.advance().unwrap().clone());
        }

        let token = if let Some(t) = self.peek_token() {
            t
        } else if let Some(t) = self.previous() {
            t
        } else {
            return DUMMY_TOKEN.with(|token| {
                Err(ParserError::with_token_span(
                    message,
                    token.line,
                    token.column,
                    token.length,
                    &self.get_source_text()
                ))
            });
        };
            
        Err(ParserError::with_token_span(
            message,
            token.line,
            token.column,
            token.length,
            &self.get_source_text()
        ))
    }

    pub fn previous_line_terminator(&self) -> bool {
        if let Some(prev) = self.previous() {
            if let Some(curr) = self.peek_token() {
                return prev.line < curr.line;
            }
        }
        false
    }

    // TODO delete
    pub fn identifier_name(&self, token: &Token) -> ParseResult<Box<str>> {
        match &token.token_type {
            TokenType::Identifier(name) => Ok(name.clone().into_boxed_str()),
            _ => Err(ParserError::with_token_span(
                "Expected identifier",
                token.line,
                token.column,
                token.length,
                &self.get_source_text()
            )),
        }
    }

    pub fn expect_identifier(&mut self, message: &str) -> ParseResult<Box<str>> {
        // Create a binding for the error case
        let binding = Token::new(TokenType::EOF, 0, 0, 0);
        
        // Get the token, handling the case where there might not be one
        let token = match self.advance() {
            Some(t) => t,
            None => {
                let last = self.previous().unwrap_or(&binding);
                return Err(ParserError::with_token_span(
                    message,
                    last.line,
                    last.column,
                    last.length,
                    &self.get_source_text()
                ));
            }
        };

        match &token.token_type {
            TokenType::Identifier(name) => Ok(name.clone().into_boxed_str()),
            TokenType::Default => Ok("default".into()),
            TokenType::As => Ok("as".into()),
            TokenType::For => Ok("for".into()),
            TokenType::Target => Ok("target".into()),
            TokenType::From => Ok("from".into()),
            TokenType::Class => Ok("class".into()),
            TokenType::Get => Ok("get".into()),
            TokenType::Set => Ok("set".into()),
            _ => Err(ParserError::with_token_span(
                &format!("Expected identifier, found {:?}", token.token_type),
                token.line,
                token.column,
                token.length,
                &self.get_source_text()
            )),
        }
    }

    // Error helper
    pub fn error_unexpected(&self, message: &str) -> ParserError {
        let binding = Token::new(TokenType::EOF, 0, 0, 0);
        let token = match self.peek_token() {
            Some(t) => t,
            None => match self.previous() {
                Some(t) => t,
                None => &binding
            }
        };
        ParserError::with_token_span(
            message, 
            token.line, 
            token.column,
            token.length,
            &self.get_source_text()
        )
    }

    pub fn get_source_text(&self) -> String {
        self.source.clone().unwrap_or_default()
    }

    // Main parse methods
    pub fn parse(&mut self) -> ParseResult<Program> {
        if self.tokens.is_empty() {
            return Ok(Program {
                source_type: SourceType::Script,
                body: Vec::new(),
                comments: Vec::new(),
            });
        }

        self.parse_program()
    }

    pub fn parse_as_module(&mut self) -> ParseResult<Program> {
        if self.tokens.is_empty() {
            return Ok(Program {
                source_type: SourceType::Module,
                body: Vec::new(),
                comments: Vec::new(),
            });
        }

        self.parse_module()
    }

    pub fn parse_single_statement(&mut self) -> ParseResult<Statement> {
        if self.tokens.is_empty() {
            return Ok(Statement::Empty);
        }

        let stmt = self.parse_statement()?;
        
        // Ensure we've consumed all tokens
        if !self.is_at_end() && !matches!(self.peek_token_type(), Some(TokenType::EOF)) {
            let token = self.peek_token().unwrap();
            return Err(ParserError::with_token_span(
                "Unexpected token after statement",
                token.line,
                token.column,
                token.length,
                &self.get_source_text()
            ));
        }
        
        Ok(stmt)
    }

    pub fn parse_single_expression(&mut self) -> ParseResult<Expression> {
        if self.tokens.is_empty() {
            return Err(ParserError::with_token_span(
                "Empty input",
                0,
                0,
                0,
                &self.get_source_text()
            ));
        }

        let expr = self.parse_expression()?;
        
        // Ensure we've consumed all tokens
        if !self.is_at_end() && !matches!(self.peek_token_type(), Some(TokenType::EOF)) {
            let token = self.peek_token().unwrap();
            return Err(ParserError::with_token_span(
                "Unexpected token after expression",
                token.line,
                token.column,
                token.length,
                &self.get_source_text(),
            ));
        }
        
        Ok(expr)
    }

    pub fn parse_comment(&mut self, text: String, is_block: bool, start: usize, end: usize) {
        let comment = Comment {
            text: text.into_boxed_str(),
            is_block,
            span: (start as u32, end as u32),
        };
        self.comments.push(comment);
    }

    // Helper method to handle parsing of "enum" keyword which is reserved in strict mode
    pub fn handle_reserved_word(&self, word: &str) -> ParseResult<()> {
        if self.state.in_strict_mode {
            let reserved_words = ["implements", "interface", "package", "private", "protected", "public", "enum", "eval", "arguments"];
            
            if reserved_words.contains(&word) {
                let token = self.previous().unwrap();
                return Err(super::error::ParserError::with_token_span(
                    &format!("'{}' is a reserved word in strict mode", word),
                    token.line,
                    token.column,
                    token.length,
                    &self.get_source_text(),
                ));
            }
        }
        
        Ok(())
    }

    // Helper method to validate variable names
    pub fn validate_variable_name(&self, name: &str) -> ParseResult<()> {
        if self.state.in_strict_mode {
            if name == "eval" || name == "arguments" {
                let token = self.previous().unwrap();
                return Err(super::error::ParserError::with_token_span(
                    &format!("'{}' cannot be used as a variable name in strict mode", name),
                    token.line,
                    token.column,
                    token.length,
                    &self.get_source_text(),
                ));
            }
        }
        
        Ok(())
    }

    // Helper method to validate function parameters
    pub fn validate_function_params(&self, params: &[Expression]) -> ParseResult<()> {
        let mut seen_params = HashSet::new();
        
        for param in params {
            if let Expression::Identifier(name) = param {
                if self.state.in_strict_mode && (name.as_ref() == "eval" || name.as_ref() == "arguments") {
                    return Err(super::error::ParserError::with_token_span(
                        &format!("'{}' cannot be used as a parameter name in strict mode", name),
                        self.previous().unwrap().line,
                        self.previous().unwrap().column,
                        self.previous().unwrap().length,
                        &self.get_source_text(),
                    ));
                }
                
                if !seen_params.insert(name.clone()) {
                    return Err(super::error::ParserError::with_token_span(
                        &format!("Duplicate parameter name '{}'", name),
                        self.previous().unwrap().line,
                        self.previous().unwrap().column,
                        self.previous().unwrap().length,
                        &self.get_source_text(),
                    ));
                }
            }
        }
        
        Ok(())
    }

    // Helper method to handle octal literals in strict mode
    pub fn validate_octal_literal(&self, value: &str) -> ParseResult<()> {
        if self.state.in_strict_mode && value.starts_with('0') && !value.starts_with("0x") && !value.starts_with("0b") && !value.starts_with("0o") {
            return Err(super::error::ParserError::with_token_span(
                "Octal literals are not allowed in strict mode",
                self.previous().unwrap().line,
                self.previous().unwrap().column,
                self.previous().unwrap().length,
                &self.get_source_text(),
            ));
        }
        
        Ok(())
    }

    // Helper method to parse a list of elements separated by commas
    pub fn parse_comma_separated_list<T, F>(&mut self, terminator: &TokenType, parser_fn: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        let mut elements = Vec::new();
        
        if !self.check(terminator) {
            loop {
                elements.push(parser_fn(self)?);
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(terminator) {
                    break;
                }
            }
        }
        
        self.consume(terminator, &format!("Expected '{:?}'", terminator))?;
        Ok(elements)
    }

    // Helper method to parse arguments for function calls
    pub fn parse_arguments(&mut self) -> ParseResult<Vec<Argument>> {
        let mut args = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            loop {
                if self.match_token(&TokenType::Ellipsis) {
                    // Spread argument
                    let expr = self.parse_expression()?;
                    args.push(Argument::Spread(expr));
                } else {
                    // Regular argument
                    let expr = self.parse_expression()?;
                    args.push(Argument::Expression(expr));
                }
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(&TokenType::RightParen) {
                    break;
                }
            }
        }
        
        self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
        
        Ok(args)
    }

    // Property key parsing for object literals, class members, and destructuring patterns
    pub fn parse_property_key(&mut self) -> ParseResult<PropertyKey> {
        if self.match_token(&TokenType::LeftBracket) {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RightBracket, "Expected ']' after computed property key")?;
            Ok(PropertyKey::Computed(expr))
        } else if self.match_token(&TokenType::Hash) {
            let name = self.expect_identifier("Expected private identifier name")?;
            Ok(PropertyKey::PrivateIdentifier(name))
        } else if let Some(TokenType::StringLiteral(_)) = self.peek_token_type() {
            if let TokenType::StringLiteral(s) = self.advance().unwrap().token_type.clone() {
                Ok(PropertyKey::StringLiteral(s.into_boxed_str()))
            } else {
                unreachable!()
            }
        } else if let Some(TokenType::NumberLiteral(_)) = self.peek_token_type() {
            if let TokenType::NumberLiteral(n) = self.advance().unwrap().token_type {
                Ok(PropertyKey::NumericLiteral(n))
            } else {
                unreachable!()
            }
        } else if self.check(&TokenType::Default) {
            Ok(PropertyKey::Identifier("default".into()))
        } else if self.check(&TokenType::Get) {
            Ok(PropertyKey::Identifier("get".into()))
        }
        else if self.check(&TokenType::Set) {
            Ok(PropertyKey::Identifier("set".into()))
        } else {
            let name = self.expect_identifier("Expected property name 999")?;
            Ok(PropertyKey::Identifier(name))
        }
    }
}
