use super::prelude::*;


use crate::ast::*;
use crate::lexer::{Token, LexicalContext};
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    pub fn parse_pattern(&mut self) -> ParseResult<Expression> {
        match self.peek() {
            // Identifier pattern
            Some(Token::Identifier(_)) => {
                let name = self.expect_identifier("Expected identifier in pattern")?;
                Ok(Expression::Identifier(name))
            },
            // Object pattern: { x, y }
            Some(Token::LeftBrace) => {
                println!("In pattern");
                self.advance(); // consume '{'
                
                let mut properties = Vec::new();
                
                if !self.check(&Token::RightBrace) {
                    loop {
                        if self.match_token(&Token::Ellipsis) {
                            // Rest element
                            let argument = self.parse_pattern()?;
                            properties.push(ObjectProperty::Spread(argument));
                            
                            // Rest element must be the last one
                            if !self.check(&Token::RightBrace) {
                                return Err(parser_error_at_current!(self, "Rest element must be the last element in object pattern"));
                            }
                            break;
                        } else {
                            // Regular property
                            let key = self.with_context(LexicalContext::PropertyKey, |parser| {
                                parser.parse_property_key()
                            })?;
                            
                            // Handle shorthand: { x }
                            let (value, computed, shorthand) = if !self.check(&Token::Colon) {
                                if let PropertyKey::Identifier(name) = &key {
                                    // Shorthand property: { x }
                                    let pattern = Expression::Identifier(name.clone());
                                    
                                    // Check for default value: { x = 1 }
                                    if self.match_token(&Token::Equal) {
                                        let default = self.parse_expression()?;
                                        (Expression::Assignment {
                                            operator: AssignmentOperator::Assign,
                                            left: Box::new(pattern),
                                            right: Box::new(default),
                                        }, false, true)
                                    } else {
                                        (pattern, false, true)
                                    }
                                } else {
                                    return Err(parser_error_at_current!(self, "Invalid shorthand property in object pattern"));
                                }
                            } else {
                                // Full syntax: { key: value }
                                self.advance(); // consume ':'
                                let pattern = self.parse_pattern()?;
                                
                                // Check for default value: { key: value = 1 }
                                if self.match_token(&Token::Equal) {
                                    let default = self.parse_expression()?;
                                    (Expression::Assignment {
                                        operator: AssignmentOperator::Assign,
                                        left: Box::new(pattern),
                                        right: Box::new(default),
                                    }, matches!(key, PropertyKey::Computed(_)), false)
                                } else {
                                    (pattern, matches!(key, PropertyKey::Computed(_)), false)
                                }
                            };
                            
                            properties.push(ObjectProperty::Property {
                                key,
                                value,
                                computed,
                                shorthand,
                                kind: PropertyKind::Init,   // FIXME not true Get/Set
                            });
                        }
                        
                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                        
                        // Handle trailing comma
                        if self.check(&Token::RightBrace) {
                            break;
                        }
                    }
                }
                
                self.consume(&Token::RightBrace, "Expected '}' after object pattern")?;
                
                Ok(Expression::Object(properties))
            },
            
            // Array pattern: [x, y, z = 1]
            Some(Token::LeftBracket) => {
                self.advance(); // consume '['
                
                let mut elements = Vec::new();
                
                while !self.check(&Token::RightBracket) && !self.is_at_end() {
                    if self.match_token(&Token::Comma) {
                        // Elision (hole)
                        elements.push(ArrayElement::Hole);    // TODO could use 
                    } else {
                        if self.match_token(&Token::Ellipsis) {
                            // Rest element
                            let argument = self.parse_pattern()?;
                            elements.push(ArrayElement::Spread(Expression::Spread(Box::new(argument))));
                            
                            // Rest element must be the last one
                            if !self.check(&Token::RightBracket) {
                                if self.match_token(&Token::Comma) {
                                    if !self.check(&Token::RightBracket) {
                                        return Err(parser_error_at_current!(self, "Rest element must be the last element in array pattern"));
                                    }
                                } else {
                                    return Err(parser_error_at_current!(self, "Expected ',' or ']' after rest element in array pattern"));
                                }
                            }
                            break;
                        } else {
                            // Regular element
                            let pattern = self.parse_pattern()?;
                            
                            // Check for default value: [x = 1]
                            if self.match_token(&Token::Equal) {
                                let default = self.parse_expression()?;
                                elements.push(ArrayElement::Expression(Expression::Assignment {
                                    operator: AssignmentOperator::Assign,
                                    left: Box::new(pattern),
                                    right: Box::new(default),
                                }));
                            } else {
                                elements.push(ArrayElement::Expression(pattern));
                            }
                        }
                        
                        if !self.check(&Token::RightBracket) {
                            self.consume(&Token::Comma, "Expected ',' after array pattern element")?;
                        }
                    }
                }
                
                self.consume(&Token::RightBracket, "Expected ']' after array pattern")?;
                
                Ok(Expression::Array(elements))
            },
            
            // Assignment pattern: x = 1 (handled by the caller)
            
            _ => {
                Err(parser_error_at_current!(self, "Expected pattern"))
            }
        }
    }

}
