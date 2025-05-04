use crate::ast::*;
use crate::lexer::{Token, TokenType};
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    // Variable declarations
    pub fn parse_variable_declaration(&mut self) -> ParseResult<VariableDeclaration> {
        let token = self.advance().cloned().unwrap_or_else(|| Token::new(TokenType::EOF, 0, 0, 0));
        
        let kind = match token.token_type {
            TokenType::Var => VariableKind::Var,
            TokenType::Let => VariableKind::Let,
            TokenType::Const => VariableKind::Const,
            _ => unreachable!(),
        };
        
        // Parse first declarator (required)
        let mut declarations = vec![self.parse_variable_declarator()?];
        
        // Parse additional declarators separated by commas
        while self.match_token(&TokenType::Comma) {
            declarations.push(self.parse_variable_declarator()?);
        }

        // Consume semicolon unless we're in a for-in/of loop
        if !self.state.in_loop {
            self.consume(&TokenType::Semicolon, "Expected ';' after variable declaration")?;
        }
        
        Ok(VariableDeclaration { declarations, kind })
    }

    pub fn parse_variable_declarator(&mut self) -> ParseResult<VariableDeclarator> {
        let id = self.parse_pattern()?;
        
        // Parse optional initializer
        let init = self.match_token(&TokenType::Equal)
            .then(|| self.parse_expression())
            .transpose()?;
        
        Ok(VariableDeclarator { id, init })
    }
}
