use crate::ast::*;
use crate::lexer::TokenType;
use super::error::ParseResult;
use super::core::Parser;
use super::expressions::Precedence;

impl Parser {

    pub fn parse_class_declaration(&mut self) -> ParseResult<ClassDeclaration> {
        self.advance(); // consume 'class'
        
        let id = self.expect_identifier("Expected class name")?;
        let super_class = self.match_token(&TokenType::Extends)
            .then(|| self.parse_expression_with_precedence(Precedence::Call))
            .transpose()?;
        
        let body = self.parse_class_body()?;
        
        Ok(ClassDeclaration { id, super_class, body })
    }

    pub fn parse_class_expression(&mut self) -> ParseResult<Expression> {
        self.advance(); // consume 'class'
        
        // Optional class name for expressions
        let id = matches!(self.peek_token_type(), Some(TokenType::Identifier(_)))
            .then(|| self.expect_identifier("Expected class name"))
            .transpose()?;
        
        // Optional extends clause
        let super_class = self.match_token(&TokenType::Extends)
            .then(|| self.parse_expression_with_precedence(Precedence::Call).map(Box::new))
            .transpose()?;
        
        let body = self.parse_class_body()?;
        
        Ok(Expression::Class { id, super_class, body })
    }

    pub fn parse_class_body(&mut self) -> ParseResult<Vec<ClassMember>> {
        self.consume(&TokenType::LeftBrace, "Expected '{' before class body")?;
        
        // Classes are always in strict mode
        let prev_strict = self.state.in_strict_mode;
        self.state.in_strict_mode = true;
        
        let mut body = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            // Skip empty class elements (semicolons)
            if self.match_token(&TokenType::Semicolon) {
                continue;
            }
            
            body.push(self.parse_class_member()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after class body")?;
        self.state.in_strict_mode = prev_strict;
        
        Ok(body)
    }

    pub fn parse_class_member(&mut self) -> ParseResult<ClassMember> {
        let is_static = self.match_token(&TokenType::Static);
        
        // Handle static blocks (ES2022)
        if is_static && self.check(&TokenType::LeftBrace) {
            return self.parse_static_block();
        }
        
        // Parse method modifiers
        let is_async = self.match_token(&TokenType::Async);
        let is_generator = self.match_token(&TokenType::Star);
        
        // Check for getter/setter
        let mut kind = MethodKind::Method;
        if !is_async && !is_generator {
            if self.match_token(&TokenType::Get) {
                kind = MethodKind::Getter;
            } else if self.match_token(&TokenType::Set) {
                kind = MethodKind::Setter;
            }
        }
        
        // Parse property key
        let key = self.parse_property_key()?;
        
        // Check for constructor method
        if !is_static && !is_async && !is_generator && kind == MethodKind::Method {
            if let PropertyKey::Identifier(name) = &key {
                if name.as_ref() == "constructor" {
                    let params = self.parse_function_params()?;
                    let body = self.parse_function_body(false, false)?;
                    return Ok(ClassMember::Constructor { params, body });
                }
            }
        }
        
        // Method definition
        if self.check(&TokenType::LeftParen) || is_generator || is_async {
            let params = self.parse_function_params()?;
            let body = self.parse_function_body(is_async, is_generator)?;
            
            return Ok(ClassMember::Method {
                key,
                value: MethodDefinition {
                    params,
                    body,
                    is_async,
                    is_generator,
                },
                kind,
                is_static,
            });
        }
        
        // Class field
        let value = self.match_token(&TokenType::Equal)
            .then(|| self.parse_expression())
            .transpose()?;
        
        self.consume_semicolon("Expected ';' after class field")?;
        
        Ok(ClassMember::Property {
            key,
            value,
            is_static,
        })
    }

    pub fn parse_static_block(&mut self) -> ParseResult<ClassMember> {
        self.consume(&TokenType::LeftBrace, "Expected '{' after 'static'")?;
        
        let mut body = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            body.push(self.parse_statement()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after static block")?;
        
        Ok(ClassMember::StaticBlock { body })
    }
}
