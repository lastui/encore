use crate::ast::*;
use crate::lexer::TokenType;
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    pub fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration> {
        self.advance(); // consume 'function'
        
        let is_generator = self.match_token(&TokenType::Star);
        let id = self.expect_identifier("Expected function name")?;
        
        // Save and update parser state
        let (prev_in_function, prev_allow_yield) = (self.state.in_function, self.state.allow_yield);
        self.state.in_function = true;
        self.state.allow_yield = is_generator;
        
        let params = self.parse_function_params()?;
        let body = self.parse_function_body(false, is_generator)?;
        
        // Restore previous state
        self.state.in_function = prev_in_function;
        self.state.allow_yield = prev_allow_yield;
        
        Ok(FunctionDeclaration {
            id,
            params,
            body,
            is_async: false,
            is_generator,
        })
    }

    pub fn parse_function_expression(&mut self) -> ParseResult<Expression> {
        self.advance(); // consume 'function'
        
        let is_generator = self.match_token(&TokenType::Star);
        
        // Optional function name for function expressions
        let id = matches!(self.peek_token_type(), Some(TokenType::Identifier(_)))
            .then(|| self.expect_identifier("Expected function name"))
            .transpose()?;
        
        // Save and update parser state
        let (prev_in_function, prev_allow_yield) = (self.state.in_function, self.state.allow_yield);
        self.state.in_function = true;
        self.state.allow_yield = is_generator;
        
        let params = self.parse_function_params()?;
        let body = self.parse_function_body(false, is_generator)?;
        
        // Restore previous state
        self.state.in_function = prev_in_function;
        self.state.allow_yield = prev_allow_yield;
        
        Ok(Expression::Function {
            id,
            params,
            body,
            is_async: false,
            is_generator,
        })
    }

    pub fn parse_async_function_expression(&mut self) -> ParseResult<Expression> {
        self.advance(); // consume 'async'
        self.advance(); // consume 'function'
        
        let is_generator = self.match_token(&TokenType::Star);
        
        // Optional function name for function expressions
        let id = matches!(self.peek_token_type(), Some(TokenType::Identifier(_)))
            .then(|| self.expect_identifier("Expected function name"))
            .transpose()?;
        
        // Save and update parser state
        let (prev_in_function, prev_allow_yield, prev_allow_await) = 
            (self.state.in_function, self.state.allow_yield, self.state.allow_await);
        self.state.in_function = true;
        self.state.allow_yield = is_generator;
        self.state.allow_await = true;
        
        let params = self.parse_function_params()?;
        let body = self.parse_function_body(true, is_generator)?;
        
        // Restore previous state
        self.state.in_function = prev_in_function;
        self.state.allow_yield = prev_allow_yield;
        self.state.allow_await = prev_allow_await;
        
        Ok(Expression::Function {
            id,
            params,
            body,
            is_async: true,
            is_generator,
        })
    }

    pub fn parse_function_params(&mut self) -> ParseResult<Vec<Expression>> {
        self.consume(&TokenType::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            loop {
                if self.match_token(&TokenType::Ellipsis) {
                    // Rest parameter
                    let arg = self.parse_pattern()?;
                    params.push(Expression::Spread(Box::new(arg)));
                    break; // Rest parameter must be the last one
                } else {
                    params.push(self.parse_pattern()?);
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
        
        self.consume(&TokenType::RightParen, "Expected ')' after function parameters")?;
        
        Ok(params)
    }

    pub fn parse_function_body(&mut self, _is_async: bool, _is_generator: bool) -> ParseResult<Vec<Statement>> {
        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        
        let mut body = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            body.push(self.parse_statement()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
        Ok(body)
    }

    pub fn parse_arrow_function_body(&mut self, params: Vec<Expression>, is_async: bool) -> ParseResult<Expression> {
        // Save and update parser state
        let (prev_in_function, prev_allow_await) = (self.state.in_function, self.state.allow_await);
        self.state.in_function = true;
        self.state.allow_await = is_async;
        
        let body = if self.check(&TokenType::LeftBrace) {
            // Block body
            let statements = self.parse_function_body(is_async, false)?;
            ArrowFunctionBody::Block(statements)
        } else {
            // Expression body
            let expr = self.parse_expression()?;
            ArrowFunctionBody::Expression(Box::new(expr))
        };
        
        // Restore previous state
        self.state.in_function = prev_in_function;
        self.state.allow_await = prev_allow_await;
        
        Ok(Expression::ArrowFunction {
            params,
            body,
            is_async,
        })
    }

    pub fn parse_async_function_declaration(&mut self) -> ParseResult<FunctionDeclaration> {
        self.advance(); // consume 'async'
        self.consume(&TokenType::Function, "Expected 'function' after 'async'")?;
        
        let is_generator = self.match_token(&TokenType::Star);
        let id = self.expect_identifier("Expected function name")?;
        
        // Save and update parser state
        let (prev_in_function, prev_allow_yield, prev_allow_await) = 
            (self.state.in_function, self.state.allow_yield, self.state.allow_await);
        self.state.in_function = true;
        self.state.allow_yield = is_generator;
        self.state.allow_await = true;
        
        let params = self.parse_function_params()?;
        let body = self.parse_function_body(true, is_generator)?;
        
        // Restore previous state
        self.state.in_function = prev_in_function;
        self.state.allow_yield = prev_allow_yield;
        self.state.allow_await = prev_allow_await;
        
        Ok(FunctionDeclaration {
            id,
            params,
            body,
            is_async: true,
            is_generator,
        })
    }

    // Helper method to check if we're looking at an async function
    pub fn is_async_function(&self) -> bool {
        if let Some(TokenType::Async) = self.peek_token_type() {
            if let Some(next_token) = self.tokens.get(self.current + 1) {
                return matches!(next_token.token_type, TokenType::Function);
            }
        }
        false
    }

    pub fn is_arrow_function_parameters(&mut self) -> (bool, bool) {
        let start_pos = self.current;
        
        if let Some(TokenType::Identifier(_)) = self.peek_token_type() {
            self.advance();
            let is_arrow = self.check(&TokenType::Arrow);
            self.current = start_pos;
            if is_arrow {
                return (true, false);
            }
        }

        if self.match_token(&TokenType::Ellipsis) {
            if let Some(TokenType::Identifier(_)) = self.peek_token_type() {
                self.advance(); 
                if self.match_token(&TokenType::RightParen) && self.check(&TokenType::Arrow) {
                    self.current = start_pos;
                    return (true, true);
                }
            }
            self.current = start_pos;
        }

        if self.match_token(&TokenType::RightParen) {
            let is_arrow = self.check(&TokenType::Arrow);
            self.current = start_pos;
            return (is_arrow, true);
        }
        
        let mut has_close_paren = false;

        loop {
            match self.parse_pattern() {
                Ok(arg) => {
                    continue;
                },
                Err(_) if self.match_token(&TokenType::Comma) => {
                    continue;
                },
                Err(_) => if self.match_token(&TokenType::RightParen) {
                    has_close_paren = true;
                    break;
                },
                Err(_) => {
                    self.current = start_pos;
                    return (false, has_close_paren)
                },
            }
            break;
        }

        if self.match_token(&TokenType::RightParen) {
            has_close_paren = true;
        }

        let is_arrow = self.check(&TokenType::Arrow);
        self.current = start_pos;
        return (is_arrow, has_close_paren)
    }

}
