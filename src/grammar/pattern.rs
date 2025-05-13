use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::literal::*;
use super::expression::*;
use super::member::*;

/// Parser for JavaScript patterns (destructuring)
pub struct PatternParser;

impl PatternParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Pattern> for PatternParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Pattern> {
        // Try to parse as object pattern
        if parser.check(&Token::LeftBrace) {
            return ObjectPatternParser::new().parse(parser).map(Pattern::ObjectPattern);
        }
        
        // Try to parse as array pattern
        if parser.check(&Token::LeftBracket) {
            return ArrayPatternParser::new().parse(parser).map(Pattern::ArrayPattern);
        }
        
        // Try to parse as rest element
        if parser.check(&Token::Ellipsis) {
            return RestElementParser::new().parse(parser).map(Pattern::RestElement);
        }
        
        // Try to parse as identifier or assignment pattern
        // First parse an identifier
        let pos = parser.save_position();

        if let Ok(ident) = IdentifierParser::new().parse(parser) {
            // Check if this is an assignment pattern
            if parser.check(&Token::Equal) {
                // Consume the equals sign
                parser.assert_consume(&Token::Equal, "Expected '=' in assignment pattern")?;
                
                // Parse the right side (must be a valid expression)
                let right = ExpressionParser::new().parse(parser)?;

                return Ok(Pattern::AssignmentPattern(AssignmentPattern {
                    left: Box::new(Pattern::Identifier(ident)),
                    right: Box::new(right),
                }));
            }
            
            // If not an assignment, return the identifier
            return Ok(Pattern::Identifier(ident));
        }
        
        // Restore position after failed identifier attempt
        parser.restore_position(pos);
        
        // Try to parse as member expression (only valid in some contexts)
        let result = MemberExpressionParser::new().parse(parser);
        if result.is_ok() {
            return result.map(Pattern::MemberExpression);
        }
        
        // If all attempts failed, return an error
        Err(parser.error_at_current("Expected a valid pattern"))
    }
}

/// Parser for object patterns
pub struct ObjectPatternParser;

impl ObjectPatternParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectPattern> for ObjectPatternParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectPattern> {
        // Consume the opening brace
        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of object pattern")?;
        
        let mut properties = Vec::new();
        
        // Parse properties until we hit the closing brace
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Check for rest element
            if parser.check(&Token::Ellipsis) {
                let rest = RestElementParser::new().parse(parser)?;
                properties.push(ObjectPatternProperty::RestElement(rest));
                
                // After rest element, we can only have a closing brace
                if !parser.check(&Token::RightBrace) {
                    // Consume comma if present
                    if parser.consume(&Token::Comma) {
                        return Err(parser.error_at_current("Rest element must be the last element in an object pattern"));
                    }
                }
                break;
            } else {
                // Parse regular property
                let property = ObjectPropertyParser::new().parse(parser)?;
                properties.push(ObjectPatternProperty::Property(property));
                
                // If there's no comma, we should be at the end
                if !parser.consume(&Token::Comma) {
                    break;
                }
            }
        }
        
        // Consume the closing brace
        parser.assert_consume(&Token::RightBrace, "Expected '}' at the end of object pattern")?;
        
        Ok(ObjectPattern {
            properties,
        })
    }
}

/// Parser for object pattern properties
pub struct ObjectPropertyParser;

impl ObjectPropertyParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectProperty> for ObjectPropertyParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectProperty> {
        // Check if we have a computed property
        let computed = parser.consume(&Token::LeftBracket);
        
        // Parse the key
        let key = if computed {
            // Parse expression inside brackets
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property key")?;
            PropertyKey::Expression(Box::new(expr))
        } else {
            // Parse identifier or literal
            match parser.peek() {
                Token::StringLiteral(_) |
                Token::NumberLiteral(_) => {
                    let literal = LiteralParser::new().parse(parser)?;
                    PropertyKey::Literal(literal)
                },
                _ => {
                    let ident = IdentifierParser::new().parse(parser)?;
                    PropertyKey::Identifier(ident)
                }
            }
        };
        
        // Check if this is a shorthand property
        let shorthand = !computed && !parser.check(&Token::Colon);
        
        // Parse the value if not shorthand
        let value = if shorthand {
            // For shorthand, the value is the same as the key
            match &key {
                PropertyKey::Identifier(ident) => {
                    // Create a new identifier with the same name
                    let name = ident.name.clone();
                    Pattern::Identifier(Identifier { name })
                },
                _ => return Err(parser.error_at_current("Invalid shorthand property in object pattern")),
            }
        } else {
            // Consume the colon
            parser.assert_consume(&Token::Colon, "Expected ':' after property key in object pattern")?;
            
            // Parse the pattern
            PatternParser::new().parse(parser)?
        };
        
        Ok(ObjectProperty {
            key,
            value,
            computed,
            shorthand,
        })
    }
}

/// Parser for array patterns
pub struct ArrayPatternParser;

impl ArrayPatternParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrayPattern> for ArrayPatternParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrayPattern> {
        parser.assert_consume(&Token::LeftBracket, "Expected '[' at start of array pattern")?;
        
        let mut elements = Vec::new();
        
        while !parser.check(&Token::RightBracket) && !parser.is_at_end() {
            if parser.consume(&Token::Comma) {
                // Handle elision (hole in the pattern)
                elements.push(None);
            } else if parser.consume(&Token::Ellipsis) {
                // Handle rest element
                let argument = Box::new(PatternParser::new().parse(parser)?);
                elements.push(Some(Pattern::RestElement(RestElement { argument })));
                
                // Rest element must be the last one
                if parser.consume(&Token::Comma) && !parser.check(&Token::RightBracket) {
                    return Err(parser.error_at_current("Rest element must be the last element in array pattern"));
                }
                break;
            } else {
                // Parse regular element
                let element = PatternParser::new().parse(parser)?;
                elements.push(Some(element));
                
                // If there's no comma, we should be at the end
                if !parser.consume(&Token::Comma) {
                    break;
                }
            }
        }
        
        parser.assert_consume(&Token::RightBracket, "Expected ']' at end of array pattern")?;
        
        Ok(ArrayPattern { elements })
    }
}


/// Parser for rest elements
pub struct RestElementParser;

impl RestElementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<RestElement> for RestElementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<RestElement> {        
        // Consume the ellipsis
        parser.assert_consume(&Token::Ellipsis, "Expected '...' for rest element")?;
        
        // Parse the argument pattern
        let argument = PatternParser::new().parse(parser)?;

        Ok(RestElement {
            argument: Box::new(argument),
        })
    }
}

/// Parser for identifiers
pub struct IdentifierParser;

impl IdentifierParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Identifier> for IdentifierParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Identifier> {        

        let name = match parser.peek() {
            Token::Identifier(name) => {
                let name = name.clone().into_boxed_str();
                parser.advance();
                Ok(name)
            },
            _ => Err(parser.error_at_current("Expected an identifier")),
        }?;

        Ok(Identifier {
            name,
        })
    }
}
