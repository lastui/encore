use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;

pub struct NewExpressionNode;

impl NewExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<NewExpression> for NewExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<NewExpression> {
        parser.assert_consume(&Token::New, "Expected 'new'")?;
        let callee = Box::new(ExpressionNode::new().parse_with_precedence(parser, Precedence::Call)?);
        let mut arguments = Vec::new();
        if parser.check(&Token::LeftParen) {
            parser.assert_consume(&Token::LeftParen, "Expected '(' after new expression")?;
            if !parser.check(&Token::RightParen) {
                arguments.push(ExpressionNode::new().parse(parser)?);
                while parser.consume(&Token::Comma) && !parser.check(&Token::RightParen) {
                    arguments.push(ExpressionNode::new().parse(parser)?);
                }
            }
            parser.assert_consume(&Token::RightParen, "Expected ')' after new expression arguments")?;
        }
        Ok(NewExpression { callee, arguments })
    }
}

impl UnparserCombinator<Expression> for NewExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, expr: &Expression) {
        if let Expression::NewExpression(new_expr) = expr {
            unparser.write_str("new");
            unparser.write_char(' ');
            ExpressionNode::new().unparse(unparser, &new_expr.callee);
            unparser.write_char('(');
            if !new_expr.arguments.is_empty() {
                for (i, arg) in new_expr.arguments.iter().enumerate() {
                    if i > 0 {
                        unparser.write_char(',');
                        unparser.space();
                    }
                    ExpressionNode::new().unparse(unparser, arg);
                }
            }
            unparser.write_char(')');
        }
    }
}