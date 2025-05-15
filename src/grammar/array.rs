use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;

pub struct ArrayExpressionNode;

impl ArrayExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrayExpression> for ArrayExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrayExpression> {
        parser.assert_consume(&Token::LeftBracket, "Expected '[' at the start of array expression")?;
        
        let mut elements = Vec::new();

        if parser.check(&Token::RightBracket) {
            parser.advance();
            return Ok(ArrayExpression { elements });
        }

        let expr_parser = ExpressionNode::new();

        loop {
            if parser.check(&Token::Comma) {
                parser.advance();
                elements.push(None);
            } else if !parser.check(&Token::RightBracket) {
                let element = expr_parser.parse_with_precedence(parser, Precedence::Comma.next())?;
                elements.push(Some(element));

                if !parser.check(&Token::Comma) {
                    break;
                }

                parser.advance();

                if parser.check(&Token::RightBracket) {
                    break;
                }
            } else {
                break;
            }
        }
        
        parser.assert_consume(&Token::RightBracket, "Expected ']' at the end of array expression")?;
        
        Ok(ArrayExpression { elements })
    }
}

impl UnparserCombinator<ArrayExpression> for ArrayExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ArrayExpression) {
        unparser.write_char('[');
        
        if !node.elements.is_empty() {
            let multiline = node.elements.len() > 5;
            if multiline {
                unparser.newline();
                unparser.with_indent(|u| {
                    for (i, elem) in node.elements.iter().enumerate() {
                        if i > 0 {
                            u.write_char(',');
                            u.newline();
                        }
                        match elem {
                            Some(expr) => {
                                ExpressionNode::new().unparse(u, expr);
                            },
                            None => {
                            }
                        }
                    }
                });
                unparser.newline();
            } else {
                unparser.space();
                for (i, elem) in node.elements.iter().enumerate() {
                    if i > 0 {
                        unparser.write_char(',');
                        unparser.space();
                    }
                    match elem {
                        Some(expr) => {
                            ExpressionNode::new().unparse(unparser, expr);
                        },
                        None => {
                        }
                    }
                }
                unparser.space();
            }
        }
        
        unparser.write_char(']');
    }
}
