use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;
use super::function::*;
use super::literal::*;

pub struct PropertyNode;

impl PropertyNode {
    pub fn new() -> Self {
        Self
    }
}

/// Parser for object properties
impl ParserCombinator<Property> for PropertyNode {
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
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property key")?;
            PropertyKey::Expression(Box::new(expr))
        } else {
            // Regular property key: identifier, string, or number
            match parser.peek() {
                Token::StringLiteral(_) |
                Token::NumberLiteral(_) => {
                    let literal = LiteralNode::new().parse(parser)?;
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
                Token::Default => {
                    // Special case for 'default' as property key
                    parser.advance(); // Consume the 'default' token
                    let name = "default".to_string().into_boxed_str();
                    PropertyKey::Identifier(Identifier { name })
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
            let func_expr = FunctionExpressionNode::new().parse(parser)?;
            Box::new(Expression::FunctionExpression(func_expr))
        } else {
            // Regular property: { key: value }
            parser.assert_consume(&Token::Colon, "Expected ':' after property key")?;
            
            // Check for arrow function
            let pos = parser.save_position();
            if matches!(parser.peek(), Token::Identifier(_)) && 
               parser.peek_next(1) == &Token::Arrow {
                // This might be an arrow function
                if let Ok(arrow_func) = ArrowFunctionExpressionNode::new().parse(parser) {
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
            let expr = ExpressionNode::new().parse_with_precedence(parser, Precedence::Assignment)?;

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


impl UnparserCombinator<Property> for PropertyNode {
    fn unparse(&self, unparser: &mut Unparser, node: &Property) {
        // Handle property modifiers (get, set, async, generator)
        match node.kind {
            PropertyKind::Get => {
                unparser.write_str("get");
                unparser.write_char(' ');
            },
            PropertyKind::Set => {
                unparser.write_str("set");
                unparser.write_char(' ');
            },
            PropertyKind::Init => {
                // For async methods
                if node.method && !node.computed {
                    if let PropertyKey::Identifier(id) = &node.key {
                        if id.name.starts_with("async") && id.name.len() > 5 {
                            unparser.write_str("async");
                            unparser.write_char(' ');
                            // Continue with the rest of the method name later
                        }
                    }
                }
                
                // For generator methods
                if node.method && !node.computed {
                    if let PropertyKey::Identifier(id) = &node.key {
                        if id.name.starts_with("*") {
                            unparser.write_char('*');
                            // Continue with the rest of the method name later
                        }
                    }
                }
            }
        }
        
        // Handle the property key
        if node.computed {
            unparser.write_char('[');
            match &node.key {
                PropertyKey::Expression(expr) => {
                    ExpressionNode::new().unparse(unparser, expr);
                },
                PropertyKey::PrivateIdentifier(id) => {
                    // Handle private identifiers (class private fields/methods)
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                },
                PropertyKey::Identifier(id) => {
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    // Handle literal keys in computed properties
                    match lit {
                        crate::ast::Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        crate::ast::Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        crate::ast::Literal::BooleanLiteral(b) => unparser.write_str(if b.value { "true" } else { "false" }),
                        crate::ast::Literal::BigIntLiteral(b) => {
                            unparser.write_str(&b.value);
                            unparser.write_char('n');
                        },
                        crate::ast::Literal::NullLiteral(_) => unparser.write_str("null"),
                        crate::ast::Literal::UndefinedLiteral(_) => unparser.undefined(),
                        crate::ast::Literal::RegExpLiteral(r) => {
                            unparser.write_char('/');
                            unparser.write_str(&r.pattern);
                            unparser.write_char('/');
                            unparser.write_str(&r.flags);
                        },
                    }
                }
            }
            unparser.write_char(']');
        } else {
            match &node.key {
                PropertyKey::Identifier(id) => {
                    // For regular identifiers
                    unparser.write_str(&id.name);
                },
                PropertyKey::PrivateIdentifier(id) => {
                    // Handle private identifiers (class private fields/methods)
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    // Handle literal keys
                    match lit {
                        crate::ast::Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        crate::ast::Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        _ => unparser.write_str("\"unknown\""),
                    }
                },
                PropertyKey::Expression(_) => {
                    // This shouldn't happen for non-computed properties
                    unparser.write_str("\"error\"");
                }
            }
        }
        
        // Handle the property value
        if node.shorthand {
            // Shorthand property: { x } instead of { x: x }
            // No need to write anything else
        } else if node.method {
            // Method definition: { method() { ... } }
            ExpressionNode::new().unparse(unparser, &node.value);
        } else {
            // Regular property: { key: value }
            unparser.write_char(':');
            unparser.space();
            ExpressionNode::new().unparse(unparser, &node.value);
        }
    }
}
