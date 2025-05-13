use crate::lexer::Token;
use super::error::ParseResult;
use super::parser::Parser;

impl<'a> Parser<'a> {
    pub fn consume_semicolon(&mut self, message: &str) -> ParseResult<&Token> {
        if self.consume(&Token::Semicolon) {
            return Ok(self.peek_previous());
        }

        // Automatic Semicolon Insertion (ASI) rules
        if self.check(&Token::RightBrace) {
            return Ok(self.peek_previous());
        }
        
        if self.is_at_end() {
            return Ok(self.peek_previous());
        }

        if self.previous_line_terminator() {
            // Special case: restricted productions
            // These statements cannot be followed by a line terminator without a semicolon
            let prev = self.peek_previous();

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
                        return Err(self.error_at_current(message));
                    }
                },
                _ => {
                    return Ok(prev)
                },
            }
        }

        // Otherwise, it's an error
        Err(self.error_at_current(message))
    }

    // Helper method to check if the current token would start an expression
    fn is_expression_start(&self) -> bool {
        match self.peek() {
            Token::Identifier(_) |
            Token::NumberLiteral(_) |
            Token::StringLiteral(_) |
            Token::TemplateLiteral(_) |
            Token::RegExpLiteral(_, _) |
            Token::True |
            Token::False |
            Token::Null |
            Token::This |
            Token::LeftParen |
            Token::LeftBracket |
            Token::LeftBrace |
            Token::Function |
            Token::New |
            Token::Delete |
            Token::Typeof |
            Token::Void |
            Token::Plus |
            Token::Minus |
            Token::Bang |
            Token::Tilde => true,
            _ => false
        }
    }
}
