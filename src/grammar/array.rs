use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;

pub struct ArrayExpressionParser;

impl ArrayExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrayExpression> for ArrayExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrayExpression> {
        parser.assert_consume(&Token::LeftBracket, "Expected '[' at the start of array expression")?;
        
        let mut elements = Vec::new();
        
        while !parser.check(&Token::RightBracket) && !parser.is_at_end() {
            if parser.consume(&Token::Comma) {
                // Handle elision (hole in the array)
                elements.push(None);
            } else {
                // Parse regular element
                let element = ExpressionParser::new().parse(parser)?;
                elements.push(Some(element));
                
                // If there's no comma, we should be at the end
                if !parser.consume(&Token::Comma) {
                    break;
                }
                
                // If we see a right bracket after a comma, it's a trailing comma
                if parser.check(&Token::RightBracket) {
                    break;
                }
            }
        }
        
        parser.assert_consume(&Token::RightBracket, "Expected ']' at the end of array expression")?;
        
        Ok(ArrayExpression { elements })
    }
}
