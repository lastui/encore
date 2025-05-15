use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::literal::*;
use super::expression::*;

pub struct PatternNode;

impl PatternNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Pattern> for PatternNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Pattern> {
        // Try to parse as object pattern
        if parser.check(&Token::LeftBrace) {
            return ObjectPatternNode::new().parse(parser).map(Pattern::ObjectPattern);
        }
        
        // Try to parse as array pattern
        if parser.check(&Token::LeftBracket) {
            return ArrayPatternNode::new().parse(parser).map(Pattern::ArrayPattern);
        }
        
        // Try to parse as rest element
        if parser.check(&Token::Ellipsis) {
            return RestElementNode::new().parse(parser).map(Pattern::RestElement);
        }
        
        // Try to parse as identifier or assignment pattern
        // First parse an identifier
        let pos = parser.save_position();

        if let Ok(ident) = IdentifierNode::new().parse(parser) {
            // Check if this is an assignment pattern
            if parser.check(&Token::Equal) {
                // Consume the equals sign
                parser.assert_consume(&Token::Equal, "Expected '=' in assignment pattern")?;
                
                // Parse the right side (must be a valid expression)
                let right = ExpressionNode::new().parse(parser)?;

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
        let result = MemberPatternNode::new().parse(parser);
        if result.is_ok() {
            return result.map(Pattern::MemberExpression);
        }
        
        // If all attempts failed, return an error
        Err(parser.error_at_current("Expected a valid pattern"))
    }
}


pub struct MemberPatternNode;

impl MemberPatternNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<MemberExpression> for MemberPatternNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<MemberExpression> {
        let object = ExpressionNode::new().parse(parser)?;
        
        // Parse the property access (without optional chaining)
        let (property, computed) = if parser.consume(&Token::LeftBracket) {
            // Computed property access: obj[expr]
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property")?;
            (MemberProperty::Expression(Box::new(expr)), true)
        } else if parser.consume(&Token::Dot) {
            // Static property access: obj.prop
            // At this point, we should be directly at the identifier
            if let Token::Identifier(_) = parser.peek() {
                let ident = IdentifierNode::new().parse(parser)?;
                (MemberProperty::Identifier(ident), false)
            } else if let Token::Default = parser.peek() {
                // Special case for 'default' as property name
                parser.advance(); // Consume the 'default' token
                let name = "default".to_string().into_boxed_str();
                (MemberProperty::Identifier(Identifier { name }), false)
            } else {
                return Err(parser.error_at_current("Expected identifier after '.'"));
            }
        } else {
            // If we're here, we're expecting a direct property access without a dot
            if let Token::Identifier(_) = parser.peek() {
                let ident = IdentifierNode::new().parse(parser)?;
                (MemberProperty::Identifier(ident), false)
            } else if let Token::Default = parser.peek() {
                // Special case for 'default' as property name
                parser.advance(); // Consume the 'default' token
                let name = "default".to_string().into_boxed_str();
                (MemberProperty::Identifier(Identifier { name }), false)
            } else {
                return Err(parser.error_at_current("Expected '.' or '[' in member pattern"));
            }
        };

        Ok(MemberExpression {
            object: Box::new(object),
            property,
            computed,
            optional: false,
        })
    }
}


/// Parser for object patterns
pub struct ObjectPatternNode;

impl ObjectPatternNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectPattern> for ObjectPatternNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectPattern> {
        // Consume the opening brace
        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of object pattern")?;
        
        let mut properties = Vec::new();
        
        // Parse properties until we hit the closing brace
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Check for rest element
            if parser.check(&Token::Ellipsis) {
                let rest = RestElementNode::new().parse(parser)?;
                properties.push(ObjectPatternProperty::RestElement(rest));
                
                // After rest element, allow a trailing comma (ES2018+)
                parser.consume(&Token::Comma);
                break;
            } else {
                // Parse regular property
                let property = ObjectPropertyNode::new().parse(parser)?;
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
pub struct ObjectPropertyNode;

impl ObjectPropertyNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ObjectProperty> for ObjectPropertyNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ObjectProperty> {
        // Check if we have a computed property
        let computed = parser.consume(&Token::LeftBracket);
        
        // Parse the key
        let key = if computed {
            // Parse expression inside brackets
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property key")?;
            PropertyKey::Expression(Box::new(expr))
        } else {
            // Parse identifier or literal
            match parser.peek() {
                Token::StringLiteral(_) |
                Token::NumberLiteral(_) => {
                    let literal = LiteralNode::new().parse(parser)?;
                    PropertyKey::Literal(literal)
                },
                Token::Default => {
                    // Special case for 'default' as property key
                    parser.advance(); // Consume the 'default' token
                    let name = "default".to_string().into_boxed_str();
                    PropertyKey::Identifier(Identifier { name })
                },
                _ => {
                    let ident = IdentifierNode::new().parse(parser)?;
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
            PatternNode::new().parse(parser)?
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
pub struct ArrayPatternNode;

impl ArrayPatternNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrayPattern> for ArrayPatternNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrayPattern> {
        parser.assert_consume(&Token::LeftBracket, "Expected '[' at start of array pattern")?;
        
        let mut elements = Vec::new();
        
        while !parser.check(&Token::RightBracket) && !parser.is_at_end() {
            if parser.consume(&Token::Comma) {
                // Handle elision (hole in the pattern)
                elements.push(None);
            } else if parser.consume(&Token::Ellipsis) {
                // Handle rest element
                let argument = Box::new(PatternNode::new().parse(parser)?);
                elements.push(Some(Pattern::RestElement(RestElement { argument })));
                
                // Rest element must be the last one
                if parser.consume(&Token::Comma) && !parser.check(&Token::RightBracket) {
                    return Err(parser.error_at_current("Rest element must be the last element in array pattern"));
                }
                break;
            } else {
                // Parse regular element
                let element = PatternNode::new().parse(parser)?;
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
pub struct RestElementNode;

impl RestElementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<RestElement> for RestElementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<RestElement> {        
        // Consume the ellipsis
        parser.assert_consume(&Token::Ellipsis, "Expected '...' for rest element")?;
        
        // Parse the argument pattern
        let argument = PatternNode::new().parse(parser)?;

        Ok(RestElement {
            argument: Box::new(argument),
        })
    }
}

/// Parser for identifiers
pub struct IdentifierNode;

impl IdentifierNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Identifier> for IdentifierNode {
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

// Main pattern unparser
impl UnparserCombinator<Pattern> for PatternNode {
    fn unparse(&self, unparser: &mut Unparser, node: &Pattern) {
        match node {
            Pattern::Identifier(ident) => {
                unparser.write_str(&ident.name);
            },
            Pattern::ObjectPattern(pattern) => {
                ObjectPatternNode::new().unparse(unparser, pattern);
            },
            Pattern::ArrayPattern(pattern) => {
                ArrayPatternNode::new().unparse(unparser, pattern);
            },
            Pattern::RestElement(rest) => {
                RestElementNode::new().unparse(unparser, rest);
            },
            Pattern::AssignmentPattern(pattern) => {
                // Left side (typically an identifier)
                match &*pattern.left {
                    Pattern::Identifier(ident) => {
                        unparser.write_str(&ident.name);
                    },
                    _ => {
                        PatternNode::new().unparse(unparser, &pattern.left);
                    }
                }
                
                // Equals sign and default value
                unparser.space();
                unparser.write_char('=');
                unparser.space();
                ExpressionNode::new().unparse(unparser, &pattern.right);
            },
            Pattern::MemberExpression(expr) => {
                MemberPatternNode::new().unparse(unparser, expr);
            }
        }
    }
}

// Member expression pattern unparser
impl UnparserCombinator<MemberExpression> for MemberPatternNode {
    fn unparse(&self, unparser: &mut Unparser, node: &MemberExpression) {
        // Unparse the object part
        ExpressionNode::new().unparse(unparser, &node.object);
        
        // Unparse the property access
        match &node.property {
            MemberProperty::Identifier(ident) => {
                // Static property access: obj.prop
                unparser.write_char('.');
                unparser.write_str(&ident.name);
            },
            MemberProperty::PrivateIdentifier(id) => {
                // Handle private identifiers (class private fields/methods)
                unparser.write_char('#');
                unparser.write_str(&id.name);
            },
            MemberProperty::Expression(expr) => {
                // Computed property access: obj[expr]
                unparser.write_char('[');
                ExpressionNode::new().unparse(unparser, expr);
                unparser.write_char(']');
            }
        }
    }
}

// Object pattern unparser
impl UnparserCombinator<ObjectPattern> for ObjectPatternNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ObjectPattern) {
        unparser.write_char('{');
        
        if !node.properties.is_empty() {
            let multiline = node.properties.len() > 3;
            
            if multiline {
                unparser.newline();
                unparser.with_indent(|u| {
                    // Process all properties
                    for (i, prop) in node.properties.iter().enumerate() {
                        if i > 0 {
                            u.write_char(',');
                            u.newline();
                        }
                        
                        match prop {
                            ObjectPatternProperty::Property(property) => {
                                ObjectPropertyNode::new().unparse(u, property);
                            },
                            ObjectPatternProperty::RestElement(rest) => {
                                RestElementNode::new().unparse(u, rest);
                            }
                        }
                    }
                });
                unparser.newline();
            } else {
                // Compact format for few properties
                unparser.space();
                
                for (i, prop) in node.properties.iter().enumerate() {
                    if i > 0 {
                        unparser.write_char(',');
                        unparser.space();
                    }
                    
                    match prop {
                        ObjectPatternProperty::Property(property) => {
                            ObjectPropertyNode::new().unparse(unparser, property);
                        },
                        ObjectPatternProperty::RestElement(rest) => {
                            RestElementNode::new().unparse(unparser, rest);
                        }
                    }
                }
                
                unparser.space();
            }
        }
        
        unparser.write_char('}');
    }
}

// Object property pattern unparser
impl UnparserCombinator<ObjectProperty> for ObjectPropertyNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ObjectProperty) {
        // Handle the property key
        if node.computed {
            unparser.write_char('[');
            match &node.key {
                PropertyKey::Expression(expr) => {
                    ExpressionNode::new().unparse(unparser, expr);
                },
                PropertyKey::Identifier(id) => {
                    unparser.write_str(&id.name);
                },
                PropertyKey::PrivateIdentifier(id) => {
                    // Handle private identifiers (class private fields/methods)
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    // Handle literal keys in computed properties
                    match lit {
                        Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        _ => unparser.write_str("\"unknown\""),
                    }
                }
            }
            unparser.write_char(']');
        } else {
            match &node.key {
                PropertyKey::Identifier(id) => {
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    // Handle literal keys
                    match lit {
                        Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        _ => unparser.write_str("\"unknown\""),
                    }
                },
                PropertyKey::PrivateIdentifier(id) => {
                    // Handle private identifiers (class private fields/methods)
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                },
                PropertyKey::Expression(_) => {
                    // This shouldn't happen for non-computed properties
                    unparser.write_str("\"error\"");
                }
            }
        }
        
        // Handle the property value if not shorthand
        if !node.shorthand {
            unparser.write_char(':');
            unparser.space();
            PatternNode::new().unparse(unparser, &node.value);
        }
    }
}

// Array pattern unparser
impl UnparserCombinator<ArrayPattern> for ArrayPatternNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ArrayPattern) {
        unparser.write_char('[');
        
        if !node.elements.is_empty() {
            let multiline = node.elements.len() > 5;
            
            if multiline {
                unparser.newline();
                unparser.with_indent(|u| {
                    // Process all elements
                    for (i, elem) in node.elements.iter().enumerate() {
                        if i > 0 {
                            u.write_char(',');
                            u.newline();
                        }
                        
                        match elem {
                            Some(pattern) => {
                                PatternNode::new().unparse(u, pattern);
                            },
                            None => {
                                // Empty slot (elision)
                            }
                        }
                    }
                });
                unparser.newline();
            } else {
                // Compact format for few elements
                unparser.space();
                
                for (i, elem) in node.elements.iter().enumerate() {
                    if i > 0 {
                        unparser.write_char(',');
                        unparser.space();
                    }
                    
                    match elem {
                        Some(pattern) => {
                            PatternNode::new().unparse(unparser, pattern);
                        },
                        None => {
                            // Empty slot (elision)
                        }
                    }
                }
                
                unparser.space();
            }
        }
        
        unparser.write_char(']');
    }
}

// Rest element unparser
impl UnparserCombinator<RestElement> for RestElementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &RestElement) {
        unparser.write_str("...");
        PatternNode::new().unparse(unparser, &node.argument);
    }
}

// Identifier unparser
impl UnparserCombinator<Identifier> for IdentifierNode {
    fn unparse(&self, unparser: &mut Unparser, node: &Identifier) {
        unparser.write_str(&node.name);
    }
}
