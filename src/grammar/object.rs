use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;
use super::function::*;
use super::literal::*;

/// Parser for object expressions
pub struct ObjectExpressionParser;

impl ObjectExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectExpression> for ObjectExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectExpression> {

        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of object expression")?;
        
        let mut properties = Vec::new();

        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {

            let property = PropertyParser::new().parse(parser)?;
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

/// Parser for object properties
pub struct PropertyParser;

impl PropertyParser {
    pub fn new() -> Self {
        Self
    }

    fn parse_property_value(&self, parser: &mut Parser) -> ParseResult<Expression> {
        ExpressionParser::new().parse_with_precedence(parser, Precedence::Assignment)
    }

}


/// Parser for object properties
impl ParserCombinator<Property> for PropertyParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Property> {
        // Check for special property types (getter, setter, async, generator)
        let mut method = false;
        let mut kind = PropertyKind::Init;
        
        // Parse property modifiers
        if parser.consume(&Token::Get) {
            kind = PropertyKind::Get;
        } else if parser.consume(&Token::Set) {
            kind = PropertyKind::Set;
        } else if parser.consume(&Token::Async) {
            method = true;
        } else if parser.consume(&Token::Star) {
            method = true;
        }
        
        // Parse the property key (computed or not)
        let computed = parser.consume(&Token::LeftBracket);
        
        let key = if computed {
            // Computed property key: [expr]
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property key")?;
            PropertyKey::Expression(Box::new(expr))
        } else {
            // Regular property key: identifier, string, or number
            match parser.peek() {
                Token::StringLiteral(_) |
                Token::NumberLiteral(_) => {
                    // String or number literal as key
                    let literal = LiteralParser::new().parse(parser)?;
                    PropertyKey::Literal(literal)
                },
                Token::Identifier(_) => {
                    // Identifier as key
                    let name = match parser.peek() {
                        Token::Identifier(ident) => ident.clone(),
                        _ => unreachable!()
                    };
                    parser.advance(); // Consume the identifier
                    PropertyKey::Identifier(Identifier { name: name.into() })
                },
                _ => return Err(parser.error_at_current("Expected property name"))
            }
        };
        
        // Check if this is a method (has parentheses after the key)
        if parser.check(&Token::LeftParen) {
            method = true;
        }
        
        // Check if this is a shorthand property (no colon after key)
        let shorthand = !computed && !method && !parser.check(&Token::Colon) && 
                        matches!(kind, PropertyKind::Init);
        
        // Parse the property value
        let value = if shorthand {
            // Shorthand property: { x } is equivalent to { x: x }
            match &key {
                PropertyKey::Identifier(ident) => {
                    Box::new(Expression::Identifier(Identifier { name: ident.name.clone() }))
                },
                _ => return Err(parser.error_at_current("Invalid shorthand property"))
            }
        } else if method {
            // Method definition: { method() { ... } }
            parser.assert_consume(&Token::LeftParen, "Expected '(' after method name")?;
            let func_expr = FunctionExpressionParser::new().parse(parser)?;
            Box::new(Expression::FunctionExpression(func_expr))
        } else {
            // Regular property: { key: value }
            parser.assert_consume(&Token::Colon, "Expected ':' after property key")?;
            
            // Check for arrow function
            let pos = parser.save_position();
            if matches!(parser.peek(), Token::Identifier(_)) && 
               parser.peek_next(1) == &Token::Arrow {
                // This might be an arrow function
                if let Ok(arrow_func) = ArrowFunctionExpressionParser::new().parse(parser) {
                    return Ok(Property {
                        key,
                        value: Box::new(Expression::ArrowFunctionExpression(arrow_func)),
                        kind,
                        method,
                        shorthand,
                        computed,
                    });
                }
                parser.restore_position(pos);
            }
            
            // Parse the value as an expression
            let expr = self.parse_property_value(parser)?;
            Box::new(expr)
        };
        
        Ok(Property {
            key,
            value,
            kind,
            method,
            shorthand,
            computed,
        })
    }
}
