use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;

pub struct AwaitExpressionNode;

impl AwaitExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<AwaitExpression> for AwaitExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<AwaitExpression> {
        if !parser.allows_await() {
            return Err(parser.error_at_current("'await' expressions are only allowed within async functions and modules"));
        }

        parser.assert_consume(&Token::Await, "Expected 'await'")?;

        let argument = Box::new(ExpressionNode::new().parse(parser)?);

        Ok(AwaitExpression {
            argument,
        })
    }
}

impl UnparserCombinator<AwaitExpression> for AwaitExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &AwaitExpression) {
        unparser.write_str("await");
        unparser.write_char(' ');
        ExpressionNode::new().unparse(unparser, &node.argument);
    }
}