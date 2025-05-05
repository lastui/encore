use super::prelude::*;

use crate::ast::*;
use crate::lexer::{TokenType, LexicalContext};
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    pub fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration> {
        self.advance(); // consume 'function'
        
        let is_generator = self.match_token(&TokenType::Star);
        let id = self.expect_identifier("Expected function name")?;
        
        let params = self.parse_function_params()?;
        
        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        let body = self.parse_function_body(is_generator, false)?;
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
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
        let id = if matches!(self.peek_token_type(), Some(TokenType::Identifier(_))) {
            Some(self.expect_identifier("Expected function name")?)
        } else {
            None
        };
        
        let params = self.parse_function_params()?;

        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        let body = self.parse_function_body(is_generator, false)?;
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
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
        let id = if matches!(self.peek_token_type(), Some(TokenType::Identifier(_))) {
            Some(self.expect_identifier("Expected function name")?)
        } else {
            None
        };
        
        let params = self.parse_function_params()?;

        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        let body = self.parse_function_body(is_generator, true)?;
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
        Ok(Expression::Function {
            id,
            params,
            body,
            is_async: true,
            is_generator,
        })
    }

    pub fn parse_arrow_function_body(&mut self, params: Vec<Expression>, is_async: bool) -> ParseResult<Expression> {
        // Create a new function body context with appropriate yield/await flags

        let body = if self.check(&TokenType::LeftBrace) {
            // Block body

            let body = self.parse_function_body(false, is_async)?;

            ArrowFunctionBody::Block(body)
        } else {
            // Expression body
            let function_body_context = LexicalContext::FunctionBody { 
                allow_yield: false, 
                allow_await: is_async 
            };

            self.with_context(function_body_context, |parser| {
                let expr = parser.parse_expression()?;
                Ok(ArrowFunctionBody::Expression(Box::new(expr)))
            })?
        };

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
        
        let params = self.parse_function_params()?;

        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        let body = self.parse_function_body(is_generator, true)?;
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
        Ok(FunctionDeclaration {
            id,
            params,
            body,
            is_async: true,
            is_generator,
        })
    }

    pub fn parse_function_params(&mut self) -> ParseResult<Vec<Expression>> {
        self.consume(&TokenType::LeftParen, "Expected '(' after function name")?;
    
        // Create parameter name context with current strict mode
        let param_context = LexicalContext::ParameterName { 
            strict_mode: self.state.in_strict_mode 
        };

        self.with_context(param_context, |parser| {
            let mut params = Vec::new();
            
            if !parser.check(&TokenType::RightParen) {
                loop {
                    if parser.match_token(&TokenType::Ellipsis) {
                        // Rest parameter
                        let arg = parser.parse_pattern()?;
                        params.push(Expression::Spread(Box::new(arg)));
                        break; // Rest parameter must be the last one
                    } else {
                        params.push(parser.parse_pattern()?);
                    }
                    if !parser.match_token(&TokenType::Comma) {
                        break;
                    }
                    // Handle trailing comma
                    if parser.check(&TokenType::RightParen) {
                        break;
                    }
                }
            }
            
            parser.consume(&TokenType::RightParen, "Expected ')' after function parameters")?;
            
            Ok(params)
        })
    }

    pub fn parse_function_body(&mut self, is_async: bool, is_generator: bool) -> ParseResult<Vec<Statement>> {
        let function_body_context = LexicalContext::FunctionBody { allow_yield: is_generator, allow_await: is_async };
        self.with_context(function_body_context, |parser| {
            let mut body = Vec::new();
            while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
                body.push(parser.parse_statement()?);
            }
            Ok(body)
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
}
