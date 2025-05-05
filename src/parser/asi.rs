use super::prelude::*;

use crate::lexer::{Token, TokenType};
use super::error::{ParserError, ParseResult};
use super::core::Parser;

impl Parser {

    pub fn consume_semicolon(&mut self, message: &str) -> ParseResult<Token> {  
        // Case 1: Explicit semicolon  
        if self.match_token(&TokenType::Semicolon) {  
            return Ok(self.previous().unwrap().clone());  
        }  

        // Automatic Semicolon Insertion (ASI) rules  
          
        // Case 2: Line terminator  
        if self.previous_line_terminator() {  
            // Special case: restricted productions  
            // These statements cannot be followed by a line terminator without a semicolon  
            if let Some(prev) = self.previous() {  
                match prev.token_type {  
                    // Rule: No LineTerminator here after return/throw/yield/break/continue  
                    TokenType::Return |   
                    TokenType::Throw |   
                    TokenType::Yield |   
                    TokenType::Break |   
                    TokenType::Continue => {  
                        // Check if there's an expression after these keywords  
                        // If not, ASI applies  
                        if self.is_expression_start() {  
                            let binding = Token::new(TokenType::EOF, 0, 0, 0);  
                            let token = self.peek_token().unwrap_or_else(|| self.previous().unwrap_or(&binding));  
                            return Err(ParserError::new(message, token.line, token.column));  
                        }  
                    },  
                    // Rule: No ASI before postfix ++ or --  
                    _ if self.check(&TokenType::PlusPlus) || self.check(&TokenType::MinusMinus) => {  
                        let binding = Token::new(TokenType::EOF, 0, 0, 0);  
                        let token = self.peek_token().unwrap_or_else(|| self.previous().unwrap_or(&binding));  
                        return Err(ParserError::new(message, token.line, token.column));  
                    },  
                    _ => {}
                }  
            }  
              
            return Ok(self.previous().unwrap().clone());  
        }  
          
        // Case 3: Closing brace  
        if self.check(&TokenType::RightBrace) {  
            return Ok(self.previous().unwrap().clone());  
        }  
          
        // Case 4: End of input  
        if self.is_at_end() {  
            return Ok(self.previous().unwrap().clone());  
        }  
          
        // Case 5: The next token would cause a syntax error  
        // This is a complex case that requires looking ahead  
        // For example, in "{ 1 \n 2 }" we need to insert a semicolon after 1  
        if self.would_cause_syntax_error() {  
            return Ok(self.previous().unwrap().clone());  
        }  
          
        // Otherwise, it's an error  
        let binding = Token::new(TokenType::EOF, 0, 0, 0);  
        let token = self.peek_token().unwrap_or_else(|| self.previous().unwrap_or(&binding));  
        Err(ParserError::new(message, token.line, token.column))  
    }  
      
    // Helper method to check if the current token would start an expression  
    fn is_expression_start(&self) -> bool {  
        match self.peek_token_type() {  
            Some(TokenType::Identifier(_)) |  
            Some(TokenType::NumberLiteral(_)) |  
            Some(TokenType::StringLiteral(_)) |  
            Some(TokenType::TemplateLiteral(_)) |  
            Some(TokenType::RegExpLiteral(_, _)) |  
            Some(TokenType::True) |  
            Some(TokenType::False) |  
            Some(TokenType::Null) |  
            Some(TokenType::This) |  
            Some(TokenType::LeftParen) |  
            Some(TokenType::LeftBracket) |  
            Some(TokenType::LeftBrace) |  
            Some(TokenType::Function) |  
            Some(TokenType::New) |  
            Some(TokenType::Delete) |  
            Some(TokenType::Typeof) |  
            Some(TokenType::Void) |  
            Some(TokenType::Plus) |  
            Some(TokenType::Minus) |  
            Some(TokenType::Bang) |  
            Some(TokenType::Tilde) => true,  
            _ => false  
        }  
    }  
      
    // Helper method to check if continuing without a semicolon would cause a syntax error  
    fn would_cause_syntax_error(&self) -> bool {  
        // This is a simplified implementation  
        // A full implementation would need to look ahead and check for specific patterns  
          
        // For example, if we have "a \n (" we need to insert a semicolon  
        // because "a(" would be parsed as a function call  
        if let Some(prev_token) = self.previous() {  
            if let Some(next_token) = self.peek_token() {  
                match (&prev_token.token_type, &next_token.token_type) {  
                    // Cases where continuing would cause a syntax error  
                    (_, TokenType::LeftParen) |  
                    (_, TokenType::LeftBracket) |  
                    (_, TokenType::Plus) |  
                    (_, TokenType::Minus) |  
                    (_, TokenType::Slash) |  
                    (_, TokenType::Star) => true,  
                    _ => false  
                }  
            } else {  
                false  
            }  
        } else {  
            false  
        }  
    }
}