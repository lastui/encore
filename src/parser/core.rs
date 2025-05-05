use super::prelude::*;

use crate::ast::*;
use crate::lexer::{Token, TokenType, LexicalContext};
use super::error::{ParserError, ParseResult};
use super::state::ParserState;
use std::collections::HashSet;


pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub comments: Vec<Comment>,
    pub state: ParserState,
    pub source: Option<String>,

    context_stack: Vec<LexicalContext>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
            comments: Vec::new(),
            state: ParserState::new(),
            source: None,
            context_stack: vec![LexicalContext::Default],
        }
    }

    // Add methods to manage the context stack
    pub fn push_context(&mut self, context: LexicalContext) {
        self.context_stack.push(context);
    }
    
    pub fn pop_context(&mut self) -> Option<LexicalContext> {
        if self.context_stack.len() > 1 {
            self.context_stack.pop()
        } else {
            None
        }
    }

    pub fn current_context(&self) -> LexicalContext {
        *self.context_stack.last().unwrap_or(&LexicalContext::Default)
    }

    pub fn get_context_stack_info(&self) -> Vec<String> {
        // Get up to the last 6 entries from the context stack
        let stack_len = self.context_stack.len();
        let start_idx = if stack_len > 6 { stack_len - 6 } else { 0 };

        self.context_stack[start_idx..]
            .iter()
            .rev()
            .map(|ctx| format!("{}", ctx))
            .collect()
    }

    pub fn with_context<F, R>(&mut self, context: LexicalContext, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        let current_pos = self.current;
        
        // Only process tokens if the context has any keywords that can be identifiers
        if context.has_keywords_as_identifiers() {
            for token in self.tokens.iter_mut().skip(current_pos) {
                // Work directly with the token type without extracting text first
                if context.allows_token_as_identifier(&token.token_type) {
                    // Get the keyword text only when we know we need to convert it
                    if let Some(text) = token.token_type.keyword_text() {
                        token.token_type = TokenType::Identifier(text.to_string());
                    }
                }
            }
        }
        
        self.push_context(context);
        let result = f(self);
        self.pop_context();
        result
    }

    /*
    pub fn with_context<F, R>(&mut self, context: LexicalContext, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        let current_pos = self.current;
        for token in self.tokens.iter_mut().skip(current_pos) {
            // TODO might be improved to not need keyword_text invocation and do checks on tokens dirrectly
            if let Some(text) = token.token_type.keyword_text() {
                if context.allows_keyword_as_identifier(text) {
                    token.token_type = TokenType::Identifier(text.to_string());
                }
            }
        }
        self.push_context(context);
        let result = f(self);
        self.pop_context();
        result
    }
    */

    // Helper methods to check contexts
    pub fn is_in_loop(&self) -> bool {
        self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::LoopBody))
    }
    
    pub fn is_in_switch(&self) -> bool {
        self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::SwitchBody))
    }
    
    pub fn is_in_function(&self) -> bool {
        self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::FunctionBody { .. }))
    }
    
    pub fn allows_yield(&self) -> bool {
        if let Some(LexicalContext::FunctionBody { allow_yield, .. }) = self.context_stack.iter()
            .rev()
            .find(|ctx| matches!(ctx, LexicalContext::FunctionBody { .. }))
        {
            *allow_yield
        } else {
            false
        }
    }
    
    pub fn allows_await(&self) -> bool {
        if let Some(LexicalContext::FunctionBody { allow_await, .. }) = self.context_stack.iter()
            .rev()
            .find(|ctx| matches!(ctx, LexicalContext::FunctionBody { .. }))
        {
            *allow_await
        } else {
            false
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
            return self.consume_semicolon(message);
        } else if self.check(token_type) {
            return Ok(self.advance().unwrap().clone());
        }

        let token = if let Some(t) = self.peek_token() {
            t
        } else if let Some(t) = self.previous() {
            t
        } else {
            return DUMMY_TOKEN.with(|token| {
                Err(parser_error_at_current!(self, message))
            });
        };
            
        Err(parser_error_at_current!(self, message))
    }

    pub fn previous_line_terminator(&self) -> bool {
        if let Some(prev) = self.previous() {
            if let Some(curr) = self.peek_token() {
                return prev.line < curr.line;
            }
        }
        false
    }

    pub fn expect_identifier(&mut self, message: &str) -> ParseResult<Box<str>> {
        if let Some(TokenType::Identifier(name)) = self.peek_token_type().cloned() {
            self.advance();
            Ok(name.into_boxed_str())
        } else {            
            Err(parser_error_at_current_mut!(self, "Expected identifier"))
        }
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
            return Err(parser_error_at_current!(self, "Unexpected token after statement"));
        }
        
        Ok(stmt)
    }

    pub fn parse_single_expression(&mut self) -> ParseResult<Expression> {
        if self.tokens.is_empty() {
            return Err(parser_error_at_current!(self, "Empty input"));
        }

        let expr = self.parse_expression()?;
        
        // Ensure we've consumed all tokens
        if !self.is_at_end() && !matches!(self.peek_token_type(), Some(TokenType::EOF)) {
            let token = self.peek_token().unwrap();
            return Err(parser_error_at_current!(self, "Unexpected token after expression"));
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

    // TODO delete
    // Helper method to handle parsing of "enum" keyword which is reserved in strict mode
    pub fn handle_reserved_word(&self, word: &str) -> ParseResult<()> {
        if self.state.in_strict_mode {
            let reserved_words = ["implements", "interface", "package", "private", "protected", "public", "enum", "eval", "arguments"];
            
            if reserved_words.contains(&word) {
                let token = self.previous().unwrap();
                return Err(parser_error_at_current!(self, "'{}' is a reserved word in strict mode", word));
            }
        }
        
        Ok(())
    }

    // Helper method to validate variable names
    pub fn validate_variable_name(&self, name: &str) -> ParseResult<()> {
        if self.state.in_strict_mode {
            if name == "eval" || name == "arguments" {
                let token = self.previous().unwrap();
                return Err(parser_error_at_current!(self, "'{}' cannot be used as a variable name in strict mode", name));
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
                    return Err(parser_error_at_previous!(self, "'{}' cannot be used as a parameter name in strict mode", name));
                }
                
                if !seen_params.insert(name.clone()) {
                    return Err(parser_error_at_previous!(self, "Duplicate parameter name '{}'", name));
                }
            }
        }
        
        Ok(())
    }

    // Helper method to handle octal literals in strict mode
    pub fn validate_octal_literal(&self, value: &str) -> ParseResult<()> {
        if self.state.in_strict_mode && value.starts_with('0') && !value.starts_with("0x") && !value.starts_with("0b") && !value.starts_with("0o") {
            return Err(parser_error_at_previous!(self, "Octal literals are not allowed in strict mode"));
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
        } else {
            let name = self.expect_identifier("Expected property name 0")?;
            Ok(PropertyKey::Identifier(name))
        }
    }
}
