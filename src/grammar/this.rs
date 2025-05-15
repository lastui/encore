use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;

pub struct ThisExpressionNode;

impl ThisExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ThisExpression> for ThisExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ThisExpression> {
        parser.assert_consume(&Token::This, "Expected 'this'")?;
        Ok(ThisExpression {})
    }
}

impl UnparserCombinator<ThisExpression> for ThisExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, _node: &ThisExpression) {
        unparser.write_str("this");
    }
}
