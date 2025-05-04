use crate::ast::*;
use crate::lexer::TokenType;
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

    pub fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match self.peek_token_type() {
            // Identifier pattern
            Some(TokenType::Identifier(_)) | 
            Some(TokenType::Default) | 
            Some(TokenType::As) | 
            Some(TokenType::From) => {
                let name = self.expect_identifier("Expected identifier in pattern")?;
                Ok(Pattern::Identifier(name))
            },
            // Object pattern: { x, y }
            Some(TokenType::LeftBrace) => {
                self.advance(); // consume '{'
                
                let mut properties = Vec::new();
                
                if !self.check(&TokenType::RightBrace) {
                    loop {
                        if self.match_token(&TokenType::Ellipsis) {
                            // Rest element
                            let argument = self.parse_pattern()?;
                            properties.push(ObjectPatternProperty::Rest(Box::new(argument)));
                            
                            // Rest element must be the last one
                            if !self.check(&TokenType::RightBrace) {
                                return Err(self.error_unexpected("Rest element must be the last element in object pattern"));
                            }
                            break;
                        } else {
                            // Regular property
                            let key = self.parse_property_key()?;
                            
                            // Handle shorthand: { x }
                            let (value, computed, shorthand) = if !self.check(&TokenType::Colon) {
                                if let PropertyKey::Identifier(name) = &key {
                                    // Shorthand property: { x }
                                    let pattern = Pattern::Identifier(name.clone());
                                    
                                    // Check for default value: { x = 1 }
                                    if self.match_token(&TokenType::Equal) {
                                        let default = self.parse_expression()?;
                                        (Pattern::AssignmentPattern {
                                            left: Box::new(pattern),
                                            right: default,
                                        }, false, true)
                                    } else {
                                        (pattern, false, true)
                                    }
                                } else {
                                    return Err(self.error_unexpected("Invalid shorthand property in object pattern"));
                                }
                            } else {
                                // Full syntax: { key: value }
                                self.advance(); // consume ':'
                                let pattern = self.parse_pattern()?;
                                
                                // Check for default value: { key: value = 1 }
                                if self.match_token(&TokenType::Equal) {
                                    let default = self.parse_expression()?;
                                    (Pattern::AssignmentPattern {
                                        left: Box::new(pattern),
                                        right: default,
                                    }, matches!(key, PropertyKey::Computed(_)), false)
                                } else {
                                    (pattern, matches!(key, PropertyKey::Computed(_)), false)
                                }
                            };
                            
                            properties.push(ObjectPatternProperty::Property {
                                key,
                                value,
                                computed,
                                shorthand,
                            });
                        }
                        
                        if !self.match_token(&TokenType::Comma) {
                            break;
                        }
                        
                        // Handle trailing comma
                        if self.check(&TokenType::RightBrace) {
                            break;
                        }
                    }
                }
                
                self.consume(&TokenType::RightBrace, "Expected '}' after object pattern")?;
                
                Ok(Pattern::ObjectPattern(properties))
            },
            
            // Array pattern: [x, y, z = 1]
            Some(TokenType::LeftBracket) => {
                self.advance(); // consume '['
                
                let mut elements = Vec::new();
                
                while !self.check(&TokenType::RightBracket) && !self.is_at_end() {
                    if self.match_token(&TokenType::Comma) {
                        // Elision (hole)
                        elements.push(None);
                    } else {
                        if self.match_token(&TokenType::Ellipsis) {
                            // Rest element
                            let argument = self.parse_pattern()?;
                            elements.push(Some(Pattern::RestElement(Box::new(argument))));
                            
                            // Rest element must be the last one
                            if !self.check(&TokenType::RightBracket) {
                                if self.match_token(&TokenType::Comma) {
                                    if !self.check(&TokenType::RightBracket) {
                                        return Err(self.error_unexpected("Rest element must be the last element in array pattern"));
                                    }
                                } else {
                                    return Err(self.error_unexpected("Expected ',' or ']' after rest element in array pattern"));
                                }
                            }
                            break;
                        } else {
                            // Regular element
                            let pattern = self.parse_pattern()?;
                            
                            // Check for default value: [x = 1]
                            if self.match_token(&TokenType::Equal) {
                                let default = self.parse_expression()?;
                                elements.push(Some(Pattern::AssignmentPattern {
                                    left: Box::new(pattern),
                                    right: default,
                                }));
                            } else {
                                elements.push(Some(pattern));
                            }
                        }
                        
                        if !self.check(&TokenType::RightBracket) {
                            self.consume(&TokenType::Comma, "Expected ',' after array pattern element")?;
                        }
                    }
                }
                
                self.consume(&TokenType::RightBracket, "Expected ']' after array pattern")?;
                
                Ok(Pattern::ArrayPattern(elements))
            },
            
            // Assignment pattern: x = 1 (handled by the caller)
            
            _ => {
                Err(self.error_unexpected("Expected pattern"))
            }
        }
    }
    // Helper method to convert an expression to a pattern (for arrow function parameters)
    pub fn expression_to_pattern(&self, expr: Expression) -> ParseResult<Pattern> {
        match expr {
            Expression::Identifier(name) => Ok(Pattern::Identifier(name)),
            Expression::Object(props) => {
                // Convert object expression to object pattern
                let mut pattern_props = Vec::new();
                
                for prop in props {
                    match prop {
                        ObjectProperty::Property { key, value, computed, shorthand, .. } => {
                            if let Expression::Identifier(name) = value {
                                pattern_props.push(ObjectPatternProperty::Property {
                                    key,
                                    value: Pattern::Identifier(name),
                                    computed,
                                    shorthand,
                                });
                            } else {
                                return Err(self.error_unexpected("Invalid object pattern"));
                            }
                        },
                        ObjectProperty::Spread(expr) => {
                            if let Expression::Identifier(name) = expr {
                                pattern_props.push(ObjectPatternProperty::Rest(
                                    Box::new(Pattern::Identifier(name))
                                ));
                            } else {
                                return Err(self.error_unexpected("Invalid rest pattern"));
                            }
                        },
                        _ => return Err(self.error_unexpected("Invalid object pattern")),
                    }
                }
                
                Ok(Pattern::ObjectPattern(pattern_props))
            },
            Expression::Array(elements) => {
                // Convert array expression to array pattern
                let mut pattern_elements = Vec::new();
                
                for element in elements {
                    match element {
                        None => pattern_elements.push(None),
                        Some(ArrayElement::Expression(expr)) => {
                            if let Expression::Identifier(name) = expr {
                                pattern_elements.push(Some(Pattern::Identifier(name)));
                            } else {
                                return Err(self.error_unexpected("Invalid array pattern"));
                            }
                        },
                        Some(ArrayElement::Spread(expr)) => {
                            if let Expression::Identifier(name) = expr {
                                pattern_elements.push(Some(Pattern::RestElement(
                                    Box::new(Pattern::Identifier(name))
                                )));
                            } else {
                                return Err(self.error_unexpected("Invalid rest pattern"));
                            }
                        },
                        Some(ArrayElement::Hole) => {
                            // Handle hole elements (like [,,,]) by adding None to the pattern elements
                            pattern_elements.push(None);
                        },
                    }
                }
                
                Ok(Pattern::ArrayPattern(pattern_elements))
            },
            _ => Err(self.error_unexpected("Invalid pattern")),
        }
    }
}
