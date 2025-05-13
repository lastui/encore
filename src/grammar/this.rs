use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;

pub struct ThisExpressionParser;

impl ThisExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ThisExpression> for ThisExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ThisExpression> {
        parser.assert_consume(&Token::This, "Expected 'this'")?;
        Ok(ThisExpression {})
    }
}