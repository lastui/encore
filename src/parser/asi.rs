use super::prelude::*;

use crate::lexer::Token;
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    pub fn consume_semicolon(&mut self, message: &str) -> ParseResult<Token> {  

        if self.match_token(&Token::Semicolon) {
            return Ok(self.peek_previous().unwrap().clone());  
        }  

        // Automatic Semicolon Insertion (ASI) rules  

        if self.check(&Token::RightBrace) {  
            return Ok(self.peek_previous().unwrap().clone());  
        }  
          
        if self.is_at_end() {  
            return Ok(self.peek_previous().unwrap().clone());  
        }  

        if self.previous_line_terminator() {

            // Special case: restricted productions  
            // These statements cannot be followed by a line terminator without a semicolon  
            if let Some(prev) = self.peek_previous() {  
                match prev {  
                    // Rule: No LineTerminator here after return/throw/yield/break/continue  
                    Token::Return |   
                    Token::Throw |   
                    Token::Yield |   
                    Token::Break |   
                    Token::Continue => {  
                        // Check if there's an expression after these keywords  
                        // If not, ASI applies
                        if !self.is_expression_start() {  
                            return Err(parser_error_at_current!(self, message));
                        }  
                    },
                    _ => {}
                }  
            }  
              
            return Ok(self.peek_previous().unwrap().clone());  
        }  

        // Otherwise, it's an error          
        Err(parser_error_at_current!(self, message))
    }  


    // Helper method to check if the current token would start an expression  
    fn is_expression_start(&self) -> bool {  
        match self.peek() {  
            Some(Token::Identifier(_)) |  
            Some(Token::NumberLiteral(_)) |  
            Some(Token::StringLiteral(_)) |  
            Some(Token::TemplateLiteral(_)) |  
            Some(Token::RegExpLiteral(_, _)) |  
            Some(Token::True) |  
            Some(Token::False) |  
            Some(Token::Null) |  
            Some(Token::This) |  
            Some(Token::LeftParen) |  
            Some(Token::LeftBracket) |  
            Some(Token::LeftBrace) |  
            Some(Token::Function) |  
            Some(Token::New) |  
            Some(Token::Delete) |  
            Some(Token::Typeof) |  
            Some(Token::Void) |  
            Some(Token::Plus) |  
            Some(Token::Minus) |  
            Some(Token::Bang) |  
            Some(Token::Tilde) => true,  
            _ => false  
        }  
    }  
}