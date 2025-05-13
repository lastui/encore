use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;

pub struct CallExpressionParser;

impl CallExpressionParser {
    pub fn new() -> Self {
        Self
    }
    
    pub fn parse_with_callee(&self, parser: &mut Parser, callee: Expression) -> ParseResult<CallExpression> {
    
        let optional = parser.consume(&Token::QuestionDot);

        if optional && !parser.check(&Token::LeftParen) {
            return Err(parser.error_at_current("Expected '(' after optional chaining operator in function call"));
        }
        
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut arguments = Vec::new();

        if !parser.check(&Token::RightParen) {
            arguments.push(ExpressionParser::new().parse(parser)?);

            while parser.consume(&Token::Comma) && !parser.check(&Token::RightParen) {
                arguments.push(ExpressionParser::new().parse(parser)?);
            }
        }

        parser.assert_consume(&Token::RightParen, "Expected ')' after function arguments")?;
        
        Ok(CallExpression {
            callee: Box::new(callee),
            arguments,
            optional,
        })
    }

}

impl ParserCombinator<CallExpression> for CallExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<CallExpression> {
        let callee = ExpressionParser::new().parse(parser)?;
        self.parse_with_callee(parser, callee)
    }
}
