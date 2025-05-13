use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;

pub struct NewExpressionParser;

impl NewExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<NewExpression> for NewExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<NewExpression> {
        parser.assert_consume(&Token::New, "Expected 'new'")?;

        // Parse the callee with appropriate precedence
        // Use a higher precedence than Call to ensure we don't consume too much
        let callee = Box::new(ExpressionParser::new().parse_with_precedence(parser, Precedence::Call)?);

        let mut arguments = Vec::new();
        
        // Only parse arguments if there are parentheses
        if parser.check(&Token::LeftParen) {
            parser.assert_consume(&Token::LeftParen, "Expected '(' after new expression")?;
            
            if !parser.check(&Token::RightParen) {
                arguments.push(ExpressionParser::new().parse(parser)?);

                while parser.consume(&Token::Comma) && !parser.check(&Token::RightParen) {
                    arguments.push(ExpressionParser::new().parse(parser)?);
                }
            }
            
            parser.assert_consume(&Token::RightParen, "Expected ')' after new expression arguments")?;
        }
        
        Ok(NewExpression {
            callee,
            arguments,
        })
    }
}
