use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;
use super::declaration::*;
use super::pattern::*;
use super::call::*;

pub struct MemberExpressionParser;

impl MemberExpressionParser {
    pub fn new() -> Self {
        Self
    }
    
    /// Parse a member expression with a given object
    pub fn parse_with_object(&self, parser: &mut Parser, object: Expression) -> ParseResult<MemberExpression> {
        // Check if this is an optional chain
        // Note: In many cases, the QuestionDot has already been consumed by the caller
        let optional = parser.consume(&Token::QuestionDot);
        
        // Parse the property access
        let (property, computed) = if parser.consume(&Token::LeftBracket) {
            // Computed property access: obj[expr] or obj?.[expr]
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property")?;
            (MemberProperty::Expression(Box::new(expr)), true)
        } else if parser.check(&Token::LeftParen) && optional {
            // Optional function call: obj?.(args)
            // Let the call expression parser handle it
            return Err(parser.error_at_current("Expected property name after optional chaining operator"));
        } else if optional || parser.consume(&Token::Dot) {
            // Static property access: obj.prop or obj?.prop
            // At this point, we should be directly at the identifier
            if let Token::Identifier(_) = parser.peek() {
                let ident = IdentifierParser::new().parse(parser)?;
                (MemberProperty::Identifier(ident), false)
            } else {
                return Err(parser.error_at_current("Expected identifier after '.' or '?.'"));
            }
        } else {
            // If we're here, we're expecting a direct property access without a dot
            // This happens when the caller has already consumed the QuestionDot
            if let Token::Identifier(_) = parser.peek() {
                let ident = IdentifierParser::new().parse(parser)?;
                (MemberProperty::Identifier(ident), false)
            } else {
                return Err(parser.error_at_current("Expected '.' or '[' in member expression"));
            }
        };

        Ok(MemberExpression {
            object: Box::new(object),
            property,
            computed,
            optional,
        })
    }

}

impl ParserCombinator<MemberExpression> for MemberExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<MemberExpression> {
        let object = ExpressionParser::new().parse(parser)?;
        self.parse_with_object(parser, object)
    }
}
