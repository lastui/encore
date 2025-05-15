use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;

pub struct YieldExpressionNode;

impl YieldExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<YieldExpression> for YieldExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<YieldExpression> {
        if !parser.allows_yield() {
            return Err(parser.error_at_current("'yield' expressions are only allowed within generator functions"));
        }

        parser.assert_consume(&Token::Yield, "Expected 'yield'")?;

        let delegate = parser.consume(&Token::Star);

        let argument = if parser.check(&Token::Semicolon) || parser.is_at_end() || 
                         parser.check(&Token::RightBrace) || parser.check(&Token::Comma) ||
                         parser.check(&Token::RightParen) || parser.check(&Token::RightBracket) ||
                         parser.check(&Token::Colon) || parser.previous_line_terminator() {
            None
        } else {
            Some(Box::new(ExpressionNode::new().parse(parser)?))
        };
        
        Ok(YieldExpression {
            argument,
            delegate,
        })
    }
}

impl UnparserCombinator<YieldExpression> for YieldExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &YieldExpression) {
        unparser.write_str("yield");        
        if node.delegate {
            unparser.write_char('*');
        }
        if let Some(argument) = &node.argument {
            unparser.write_char(' ');
            ExpressionNode::new().unparse(unparser, argument);
        }
    }
}