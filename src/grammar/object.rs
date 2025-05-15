use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::property::*;

pub struct ObjectExpressionNode;

impl ObjectExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectExpression> for ObjectExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectExpression> {
        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of object expression")?;
        let mut properties = Vec::new();
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            let property = PropertyNode::new().parse(parser)?;
            properties.push(property.clone());
            if parser.consume(&Token::Comma) {
                if parser.check(&Token::RightBrace) {
                    break;
                }
            } else {
                break;
            }
        }
        parser.assert_consume(&Token::RightBrace, "Expected '}' at the end of object expression")?;
        Ok(ObjectExpression { properties })
    }
}

impl UnparserCombinator<ObjectExpression> for ObjectExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ObjectExpression) {
        unparser.write_char('{');
        if !node.properties.is_empty() {
            let multiline = node.properties.len() > 1;
            if multiline {
                unparser.newline();
                unparser.with_indent(|u| {
                    PropertyNode::new().unparse(u, &node.properties[0]);
                    for property in &node.properties[1..] {
                        u.write_char(',');
                        u.newline();
                        PropertyNode::new().unparse(u, property);
                    }
                });
                unparser.newline();
            } else {
                unparser.space();
                PropertyNode::new().unparse(unparser, &node.properties[0]);
                unparser.space();
            }
        }
        unparser.write_char('}');
    }
}
