use super::prelude::*;

use crate::ast::*;
use crate::lexer::{Token, LexicalContext};
use super::error::{ParserError, ParseResult};
use super::state::ParserState;
use std::collections::HashSet;


pub struct Parser {
    pub tokens: Vec<(Token, (usize, usize))>,
    pub current: usize,
    pub comments: Vec<Comment>,
    pub state: ParserState,
    pub source: Option<String>,

    context_stack: Vec<LexicalContext>,
}

impl Parser {

    pub fn new(tokens: Vec<(Token, (usize, usize))>) -> Self {
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

    pub fn get_current_position(&self) -> (usize, usize) {
        let item = if self.is_at_end() {
            self.tokens.get(self.current - 1)
        } else {
            self.tokens.get(self.current)
        };

        item.map(|(_, pos)| pos).unwrap_or(&(0,0)).clone()
    }

    // TODO FIXME this is erroneous it takes All the tokens starting from now until end not just those ones to be processed within the context scope
    pub fn with_context<F, R>(&mut self, context: LexicalContext, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
        {
            let current_pos = self.current;
            
            // Only process tokens if the context has any keywords that can be identifiers
            if context.has_keywords_as_identifiers() {
            // Iterate over tokens starting from the current position
            for (token_type, _) in self.tokens.iter_mut().skip(current_pos) {
                // Check if the token type can be treated as an identifier in the current context
                if context.allows_token_as_identifier(token_type) {
                    // Get the keyword text only when a conversion is needed
                    if let Some(text) = token_type.keyword_text() {
                        // Modify the token type in-place
                        *token_type = Token::Identifier(text.to_string());
                    }
                }
            }
        }
            
        self.push_context(context);
        let result = f(self);
        self.pop_context();
        result
    }

    // Helper methods to check contexts
    pub fn is_in_loop_body(&self) -> bool {
        self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::LoopBody))
    }

    pub fn is_in_loop_parameters(&self) -> bool {
        //self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::LoopParameters))
        matches!(self.context_stack.last(), Some(LexicalContext::LoopParameters))
    }
    
    pub fn is_in_switch(&self) -> bool {
        //self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::SwitchBody))
        matches!(self.context_stack.last(), Some(LexicalContext::SwitchBody))
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
        self.current >= self.tokens.len() || matches!(self.peek(), Some(Token::EOF))
    }

    pub fn peek_previous(&self) -> Option<&Token> {
        if self.current > 0 {
            self.tokens.get(self.current - 1).map(|(token_type, _)| token_type)
        } else {
            None
        }
    }

    pub fn peek_next(&self, offset: usize) -> Option<&Token> {
        if self.current + offset < self.tokens.len() {
            self.tokens.get(self.current + offset).map(|(token_type, _)| token_type)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current).map(|(token_type, _)| token_type)
    }

    pub fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.peek_previous()
    }

    pub fn check(&self, token_type: &Token) -> bool {
        match self.peek() {
            Some(t) => t == token_type,
            None => false,
        }
    }

    pub fn match_token(&mut self, token_type: &Token) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_any(&mut self, token_types: &[Token]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn consume(&mut self, token_type: &Token, message: &str) -> ParseResult<Token> {
        if token_type == &Token::Semicolon {
            self.consume_semicolon(message)?;
            return Ok(Token::Semicolon);
        } else if self.check(token_type) {
            return Ok(self.advance().unwrap().clone());
        }

        Err(parser_error_at_current!(self, message))
    }

    pub fn previous_line_terminator(&self) -> bool {
        if let Some((_, prev_pos)) = self.tokens.get(self.current.saturating_sub(1)) {
            if let Some((_, curr_pos)) = self.tokens.get(self.current) {
                return prev_pos.0 < curr_pos.0;
            }
        }
        false
    }

    pub fn expect_identifier(&mut self, message: &str) -> ParseResult<Box<str>> {
        if let Some(Token::Identifier(name)) = self.peek().cloned() {
            self.advance();
            Ok(name.into_boxed_str())
        } else {            
            Err(parser_error_at_current_mut!(self, message))
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
        if !self.is_at_end() && !matches!(self.peek(), Some(Token::EOF)) {
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
        if !self.is_at_end() && !matches!(self.peek(), Some(Token::EOF)) {
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
    // Helper method to validate variable names
    pub fn validate_variable_name(&self, name: &str) -> ParseResult<()> {
        if self.state.in_strict_mode {
            if name == "eval" || name == "arguments" {
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
                    // TODO should be previous so backtrack one?
                    return Err(parser_error_at_current!(self, "'{}' cannot be used as a parameter name in strict mode", name));
                }
                if !seen_params.insert(name.clone()) {
                    return Err(parser_error_at_current!(self, "Duplicate parameter name '{}'", name));
                }
            }
        }
        
        Ok(())
    }

    // Helper method to handle octal literals in strict mode
    pub fn validate_octal_literal(&self, value: &str) -> ParseResult<()> {
        if self.state.in_strict_mode && value.starts_with('0') && !value.starts_with("0x") && !value.starts_with("0b") && !value.starts_with("0o") {
            // TODO should be previous so backtrack one?
            return Err(parser_error_at_current!(self, "Octal literals are not allowed in strict mode"));
        }
        Ok(())
    }

    // Helper method to parse a list of elements separated by commas
    pub fn parse_comma_separated_list<T, F>(&mut self, terminator: &Token, parser_fn: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        let mut elements = Vec::new();
        
        if !self.check(terminator) {
            loop {
                elements.push(parser_fn(self)?);
                
                if !self.match_token(&Token::Comma) {
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
        
        if !self.check(&Token::RightParen) {
            loop {
                if self.match_token(&Token::Ellipsis) {
                    // Spread argument
                    let expr = self.parse_expression()?;
                    args.push(Argument::Spread(expr));
                } else {
                    // Regular argument
                    let expr = self.parse_expression()?;
                    args.push(Argument::Expression(expr));
                }
                
                if !self.match_token(&Token::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(&Token::RightParen) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expected ')' after arguments")?;

        Ok(args)
    }

    // Property key parsing for object literals, class members, and destructuring patterns
    pub fn parse_property_key(&mut self) -> ParseResult<PropertyKey> {
        if self.match_token(&Token::LeftBracket) {
            let expr = self.parse_expression()?;
            self.consume(&Token::RightBracket, "Expected ']' after computed property key")?;
            Ok(PropertyKey::Computed(expr))
        } else if self.match_token(&Token::Hash) {
            let name = self.expect_identifier("Expected private identifier name")?;
            Ok(PropertyKey::PrivateIdentifier(name))
        } else if let Some(Token::StringLiteral(_)) = self.peek() {
            if let Token::StringLiteral(s) = self.advance().unwrap().clone() {
                Ok(PropertyKey::StringLiteral(s.into_boxed_str()))
            } else {
                unreachable!()
            }
        } else if let Some(Token::NumberLiteral(_)) = self.peek() {
            if let Token::NumberLiteral(n) = self.advance().unwrap() {
                Ok(PropertyKey::NumericLiteral(*n))
            } else {
                unreachable!()
            }
        } else {
            let name = self.expect_identifier("Expected property name 0")?;
            Ok(PropertyKey::Identifier(name))
        }
    }
}
