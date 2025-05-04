use crate::ast::*;
use crate::lexer::{Token, TokenType, TemplatePart};
use super::error::ParseResult;
use super::core::Parser;

// Define operator precedence levels and associativity
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    None,
    Comma,           // ,
    Assignment,      // = += -= etc.
    Conditional,     // ?:
    LogicalOr,       // || ??
    LogicalAnd,      // &&
    BitwiseOr,       // |
    BitwiseXor,      // ^
    BitwiseAnd,      // &
    Equality,        // == != === !==
    Relational,      // < > <= >= in instanceof
    Shift,           // << >> >>>
    Additive,        // + -
    Multiplicative,  // * / %
    Exponentiation,  // **
    Prefix,          // ! ~ + - ++ -- typeof void delete
    Postfix,         // ++ --
    Call,            // . [] ()
    Primary
}

impl Parser {

    pub fn parse_expression(&mut self) -> ParseResult<Expression> {

//        match self.parse_expression_with_precedence(Precedence::Comma) {
//            Ok(expr) => {
//
//                match expr {
//                    Expression::ArrowFunction { params: _, body: _, is_async: _ } if self.check(&TokenType::RightParen) => {
//                        self.advance();
//                        //println!("Parsed Arrow Function!!!");
//                    },
//                    _ => {},
//                };
//
//                println!("Parsed expression: {:#?}", expr);
//                Ok(expr)
//            },
//            err => err
//        }

        //let expr =;

        //if 
        //if let Expression::ArrowFunction = &expr {
          //  println!("Consumed arrow function");
        //}

        //expr

        self.parse_expression_with_precedence(Precedence::Comma)
    }

    pub fn parse_expression_with_precedence(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        // Parse prefix expressions
        let mut expr = match self.peek_token_type() {
            // Unary prefix operators
            Some(TokenType::Bang) | 
            Some(TokenType::Tilde) | 
            Some(TokenType::Plus) | 
            Some(TokenType::Minus) |
            Some(TokenType::PlusPlus) | 
            Some(TokenType::MinusMinus) | 
            Some(TokenType::Typeof) |
            Some(TokenType::Void) | 
            Some(TokenType::Delete) => {
                self.advance();
                let operator = match self.previous().unwrap().token_type {
                    TokenType::Bang => UnaryOperator::Not,
                    TokenType::Tilde => UnaryOperator::BitwiseNot,
                    TokenType::Plus => UnaryOperator::Plus,
                    TokenType::Minus => UnaryOperator::Minus,
                    TokenType::PlusPlus => UnaryOperator::Increment,
                    TokenType::MinusMinus => UnaryOperator::Decrement,
                    TokenType::Typeof => UnaryOperator::Typeof,
                    TokenType::Void => UnaryOperator::Void,
                    TokenType::Delete => UnaryOperator::Delete,
                    _ => unreachable!(),
                };
                
                let argument = self.parse_expression_with_precedence(Precedence::Prefix)?;
                
                Expression::Unary {
                    operator,
                    argument: Box::new(argument),
                    prefix: true,
                }
            },
            // Await expression
            Some(TokenType::Await) if self.state.allow_await => {
                self.advance();
                let argument = self.parse_expression_with_precedence(Precedence::Prefix)?;
                Expression::Await(Box::new(argument))
            },
            // Yield expression
            Some(TokenType::Yield) if self.state.allow_yield => {
                self.advance();
                let delegate = self.match_token(&TokenType::Star);
                
                // Yield can be used without an argument
                let argument = if self.check(&TokenType::Semicolon) || 
                                self.check(&TokenType::RightBrace) || 
                                self.check(&TokenType::Comma) ||
                                self.check(&TokenType::RightParen) ||
                                self.check(&TokenType::Colon) ||
                                self.is_at_end() {
                    None
                } else {
                    Some(Box::new(self.parse_expression_with_precedence(Precedence::Assignment)?))
                };
                
                Expression::Yield {
                    argument,
                    delegate,
                }
            },
            // Primary expressions
            Some(TokenType::This) => {
                self.advance();
                Expression::This
            },
            Some(TokenType::Arguments) => {
                self.advance();
                Expression::Identifier("arguments".into())
            },
            Some(TokenType::Super) => {
                self.advance();
                Expression::Super
            },
            Some(TokenType::Null) => {
                self.advance();
                Expression::Literal(Literal::Null)
            },
            Some(TokenType::Undefined) => {
                self.advance();
                Expression::Literal(Literal::Undefined)
            },
            Some(TokenType::True) => {
                self.advance();
                Expression::Literal(Literal::Boolean(true))
            },
            Some(TokenType::False) => {
                self.advance();
                Expression::Literal(Literal::Boolean(false))
            },
            Some(TokenType::NumberLiteral(n)) => {
                let value = *n;
                self.advance();
                Expression::Literal(Literal::Number(value))
            },
            Some(TokenType::StringLiteral(_)) => {
                if let TokenType::StringLiteral(s) = &self.advance().unwrap().token_type {
                    Expression::Literal(Literal::String(s.clone().into_boxed_str()))
                } else {
                    unreachable!()
                }
            },
            Some(TokenType::RegExpLiteral(_, _)) => {
                if let TokenType::RegExpLiteral(pattern, flags) = self.advance().unwrap().token_type.clone() {
                    Expression::Literal(Literal::RegExp {
                        pattern: pattern.into_boxed_str(),
                        flags: flags.into_boxed_str(),
                    })
                } else {
                    unreachable!()
                }
            },
            Some(TokenType::BigIntLiteral(_)) => {
                if let TokenType::BigIntLiteral(s) = self.advance().unwrap().token_type.clone() {
                    Expression::Literal(Literal::BigInt(s.into_boxed_str()))
                } else {
                    unreachable!()
                }
            },
            Some(TokenType::TemplateLiteral(_)) => {
                if let TokenType::TemplateLiteral(parts) = self.advance().unwrap().token_type.clone() {
                    let token_line = self.previous().unwrap().line;
                    let token_column = self.previous().unwrap().column;
                    let token_length = self.previous().unwrap().length;
                    
                    let mut quasis = Vec::new();
                    let mut expressions = Vec::new();
                    
                    for (i, part) in parts.iter().enumerate() {
                        match part {
                            TemplatePart::String(s) => {
                                // Add the string part to quasis
                                quasis.push(s.clone().into_boxed_str());
                                
                                // If this is the last part and it's a string, we need to ensure
                                // we have one more expression than quasis (as per JS spec)
                                if i == parts.len() - 1 && !expressions.is_empty() {
                                    quasis.push("".into());
                                }
                            },
                            TemplatePart::Expression(expr_str) => {
                                // Create a temporary parser to parse the expression
                                let expr_str_clone = expr_str.clone();
                                let mut temp_lexer = crate::lexer::Lexer::new(&expr_str_clone);
                                match temp_lexer.scan_tokens() {
                                    Ok(tokens) => {
                                        let mut temp_parser = Parser::new(tokens);
                                        match temp_parser.parse_expression() {
                                            Ok(expr) => expressions.push(expr),
                                            Err(e) => {
                                                return Err(super::error::ParserError::with_token_span(
                                                    &format!("Invalid expression in template literal: {}", e.message),
                                                    token_line,
                                                    token_column,
                                                    token_length,
                                                    &self.get_source_text()
                                                ));
                                            }
                                        }
                                    },
                                    Err(e) => {
                                        return Err(super::error::ParserError::with_token_span(
                                            &format!("Error tokenizing expression in template literal: {}", e.message),
                                            token_line,
                                            token_column,
                                            token_length,
                                            &self.get_source_text()
                                        ));
                                    }
                                }
                                
                                // If this is the last part and it's an expression, we need to add an empty string
                                if i == parts.len() - 1 {
                                    quasis.push("".into());
                                }
                            }
                        }
                    }
                    
                    // Validate that we have one more quasi than expressions (as per JS spec)
                    if quasis.len() != expressions.len() + 1 {
                        // Add an empty string at the end if needed
                        if quasis.len() == expressions.len() {
                            quasis.push("".into());
                        } else {
                            return Err(super::error::ParserError::with_token_span(
                                &format!(
                                    "Invalid template literal: expected {} quasis but got {}", 
                                    expressions.len() + 1, 
                                    quasis.len(),
                                ),
                                token_line,
                                token_column,
                                token_length,
                                &self.get_source_text()
                            ));
                        }
                    }
                    
                    Expression::TemplateLiteral { quasis, expressions }
                } else {
                    unreachable!("Expected TemplateLiteral token")
                }
            },
            // TODO everything but Identifier hoists matches below, need a better approach to var as = e.class; scenarios
            Some(TokenType::Identifier(_)) | 
            Some(TokenType::As) | 
            Some(TokenType::Target) | 
            Some(TokenType::Class) | 
            Some(TokenType::Get) | 
            Some(TokenType::Set) | 
            Some(TokenType::From) => {
                let name = self.expect_identifier("Expected identifier in expression")?;
                if self.check(&TokenType::Arrow) {
                    let param = Pattern::Identifier(name);
                    self.advance();
                    return self.parse_arrow_function_body(vec![param], false);
                }
                Expression::Identifier(name)
            },
            Some(TokenType::LeftParen) => {

                // TODO tricky tricky

                self.advance(); // consume '('

                println!("In (");

                match self.parse_expression() {
                    Ok(expr) => {
                        println!("Parsed expr {:#?}", expr);
                        println!("Current token {:#?}", self.peek_token_type());
                        self.consume(&TokenType::RightParen, "Expected ')' after expression")?;
                        return Ok(expr);
                    },
                    Err(err) => {
                        println!("Now I go here");
                        let mut consumed_paren = false;
                        let start_pos = self.current;

                        

                        //println!("Before check is arrow");

                        let (is_arrow, arrow_consumed_right_paren) = self.is_arrow_function_parameters();

                        //println!("After check is arrow");

                        if !is_arrow {
                            println!("Not arrow at all");
                            return Err(err);
                        }

                        //println!("In Arrow Function");

                        //println!("At token {:#?}", self.peek_token_type());

                        self.current = start_pos;

                        let params = if self.match_token(&TokenType::RightParen) {
                            vec![]
                        } else {
                            let mut params = vec![];
                            loop {
                                if self.match_token(&TokenType::Ellipsis) {
                                    //println!("found ... in parameters");
                                    let arg = self.parse_pattern()?;
                                    params.push(Pattern::RestElement(Box::new(arg)));
                                    self.advance();
                                    break;
                                }

                                if self.match_token(&TokenType::RightParen) {
                                    //self.advance();
                                    break;
                                }

                                //println!("found identifier in parameters");
                                params.push(self.parse_pattern()?);

    //                            if self.match_token(&TokenType::RightParen) {
    //                                println!("found ) in parameters");
    //                                //consumed_paren = true;
    //                                //self.advance();
    //                                break;
    //                            }

                                if !self.match_token(&TokenType::Comma) {
                                    if self.match_token(&TokenType::RightParen) {
                                        break;
                                    }
                              //      println!("Not comma bailing");
                                    break;
                                    
                                }


                                //println!("At end of parameters");
                                
                            }

                            //println!("Am here");
                            //self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
                            //println!("Am there");
                            params
                        };

                        self.consume(&TokenType::Arrow, "Expected '=>' after parameters")?;

                        //println!("Currently before parsing body {:#?}", self.peek_token_type());
                        let body = self.parse_arrow_function_body(params, false)?;
                        //println!("Currently after parsing body {:#?}", self.peek_token_type());

                        //if !arrow_consumed_right_paren && self.check(&TokenType::RightParen) {
                          //  self.advance();
                        //}

                        //println!("Currently before closing ) {:#?}", self.peek_token_type());

//                            if self.match_token(&TokenType::LeftParen) {
//                                let arguments = self.parse_arguments()?;
//                                body = Expression::Call {
//                                    callee: Box::new(body),
//                                    arguments,
//                                    optional: false,
//                                };
//                            }


                        //println!("Currently immedietaly invoked {:#?}", self.peek_token_type());

                        //self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
                        //let params = if self.check(&TokenType::RightParen) {}
                        // TODO ) not consumed

                        //println!("Here all done for (");
                        //                    if self.check(&TokenType::RightParen) {
                        //                        self.advance();
                        //                        println!("Consuming dangling )");
                        //                    }
                        return Ok(body);
                        

                    },
                }

                //println!("At expr {:#?}", expr);

            },
            Some(TokenType::LeftBracket) => {
                self.advance(); // consume '['
                
                let mut elements = Vec::new();
                
                while !self.check(&TokenType::RightBracket) && !self.is_at_end() {
                    if self.match_token(&TokenType::Comma) {
                        // Elision (hole)
                        elements.push(None);
                    } else {
                        if self.match_token(&TokenType::Ellipsis) {
                            // Spread element
                            let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                            elements.push(Some(ArrayElement::Spread(expr)));
                        } else {
                            // Regular element
                            let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                            elements.push(Some(ArrayElement::Expression(expr)));
                        }
                        
                        if !self.check(&TokenType::RightBracket) {
                            self.consume(&TokenType::Comma, "Expected ',' after array element")?;
                        }
                    }
                }
                
                self.consume(&TokenType::RightBracket, "Expected ']' after array elements")?;
                
                Expression::Array(elements)
            },
            Some(TokenType::LeftBrace) => {
                self.advance(); // consume '{'
                
                let mut properties = Vec::new();
                
                while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                    if self.match_token(&TokenType::Ellipsis) {
                        // Spread property
                        let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        properties.push(ObjectProperty::Spread(expr));
                    } else {
                        // Method or property
                        let start_pos = self.current;
                        let is_async = self.match_token(&TokenType::Async);
                        let is_generator = self.match_token(&TokenType::Star);
                        
                        // Check for getter/setter
                        let mut kind = PropertyKind::Init;
                        if !is_async && !is_generator {
                            // Check if the next token is 'get' or 'set'
                            if self.check(&TokenType::Get) || self.check(&TokenType::Set) {
                                // Look ahead to see if it's followed by a colon
                                let is_property_name = if let Some(next_token) = self.tokens.get(self.current + 1) {
                                    matches!(next_token.token_type, TokenType::Colon)
                                } else {
                                    false
                                };
                                
                                // Only treat as getter/setter if not followed by a colon
                                if !is_property_name {
                                    if self.match_token(&TokenType::Get) {
                                        kind = PropertyKind::Get;
                                    } else if self.match_token(&TokenType::Set) {
                                        kind = PropertyKind::Set;
                                    }
                                }
                            }
                        }
                        
                        // Parse property key
                        let key = if self.match_token(&TokenType::LeftBracket) {
                            // Computed property key
                            let expr = self.parse_expression()?;
                            self.consume(&TokenType::RightBracket, "Expected ']' after computed property key")?;
                            PropertyKey::Computed(expr)
                        } else if self.match_token(&TokenType::Hash) {
                            // Private identifier (class fields/methods)
                            let name = self.expect_identifier("Expected private identifier name")?;
                            PropertyKey::PrivateIdentifier(name)
                        } else if let Some(TokenType::StringLiteral(_)) = self.peek_token_type() {
                            if let TokenType::StringLiteral(s) = self.advance().unwrap().token_type.clone() {
                                PropertyKey::StringLiteral(s.into_boxed_str())
                            } else {
                                unreachable!()
                            }
                        } else if let Some(TokenType::NumberLiteral(_)) = self.peek_token_type() {
                            if let TokenType::NumberLiteral(n) = self.advance().unwrap().token_type {
                                PropertyKey::NumericLiteral(n)
                            } else {
                                unreachable!()
                            }
                        } else if self.check(&TokenType::Default) {
                            self.advance();
                            PropertyKey::Identifier("default".into())
                        } else if self.check(&TokenType::Get) {
                            self.advance();
                            PropertyKey::Identifier("get".into())
                        } else if self.check(&TokenType::Set) {
                            self.advance();
                            PropertyKey::Identifier("set".into())
                        } else if self.check(&TokenType::From) {
                            self.advance();
                            PropertyKey::Identifier("from".into())
                        } else if self.check(&TokenType::As) {
                            self.advance();
                            PropertyKey::Identifier("as".into())
                        }   else if self.check(&TokenType::For) {
                            self.advance();
                            PropertyKey::Identifier("for".into())
                        } else {
                            // Identifier
                            let name = self.expect_identifier("Expected property name 1")?;
                            PropertyKey::Identifier(name)
                        };
                        
                        let computed = matches!(key, PropertyKey::Computed(_));
                        
                        // Method definition
                        if self.check(&TokenType::LeftParen) || is_generator || is_async {
                            let method_kind = match kind {
                                PropertyKind::Get => MethodKind::Getter,
                                PropertyKind::Set => MethodKind::Setter,
                                _ => MethodKind::Method,
                            };
                            
                            let params = self.parse_function_params()?;
                            let body = self.parse_function_body(is_async, is_generator)?;
                            
                            properties.push(ObjectProperty::Method {
                                key,
                                value: MethodDefinition {
                                    params,
                                    body,
                                    is_async,
                                    is_generator,
                                },
                                kind: method_kind,
                                computed,
                            });
                        } else {
                            // Regular property
                            let shorthand = !computed && 
                                            !self.check(&TokenType::Colon) && 
                                            matches!(key, PropertyKey::Identifier(_));
                            
                            let value = if shorthand {
                                if let PropertyKey::Identifier(name) = &key {
                                    Expression::Identifier(name.clone())
                                } else {
                                    unreachable!()
                                }
                            } else {
                                self.consume(&TokenType::Colon, "Expected ':' after property name")?;
                                self.parse_expression_with_precedence(Precedence::Assignment)?
                            };
                            
                            properties.push(ObjectProperty::Property {
                                key,
                                value,
                                kind,
                                computed,
                                shorthand,
                            });
                        }
                    }
                    
                    if !self.check(&TokenType::RightBrace) {
                        self.consume(&TokenType::Comma, "Expected ',' after property")?;
                        
                        // Allow trailing comma
                        if self.check(&TokenType::RightBrace) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            
                self.consume(&TokenType::RightBrace, "Expected '}' after object literal")?;
            
                Expression::Object(properties)
            },
            Some(TokenType::Function) => self.parse_function_expression()?,
            Some(TokenType::Class) => self.parse_class_expression()?,
            Some(TokenType::New) => {
                self.advance(); // consume 'new'
                
                // Handle new.target meta property
                if self.match_token(&TokenType::Dot) {
                    if let Some(TokenType::Identifier(name)) = self.peek_token_type().cloned() {
                        if name == "target" {
                            self.advance(); // consume 'target'
                            Expression::MetaProperty {
                                meta: "new".into(),
                                property: "target".into(),
                            }
                        } else {
                            let token = self.peek_token().unwrap();
                            return Err(super::error::ParserError::with_token_span(
                                "Expected 'target' after 'new.'",
                                token.line,
                                token.column,
                                token.length,
                                &self.get_source_text(),
                            ));
                        }
                    } else {
                        let token = self.peek_token().unwrap();
                        return Err(super::error::ParserError::with_token_span(
                            "Expected 'target' after 'new.'",
                            token.line,
                            token.column,
                            token.length,
                            &self.get_source_text(),
                        ));
                    }
                } else {
                    // Regular new expression
                    let callee = self.parse_expression_with_precedence(Precedence::Call)?;
                    
                    // Optional arguments
                    let arguments = if self.match_token(&TokenType::LeftParen) {
                        let mut args = Vec::new();
                        
                        if !self.check(&TokenType::RightParen) {
                            loop {
                                if self.match_token(&TokenType::Ellipsis) {
                                    // Spread argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Spread(expr));
                                } else {
                                    // Regular argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Expression(expr));
                                }
                                
                                if !self.match_token(&TokenType::Comma) {
                                    break;
                                }
                                
                                // Handle trailing comma
                                if self.check(&TokenType::RightParen) {
                                    break;
                                }
                            }
                        }
                        
                        self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
                        args
                    } else {
                        Vec::new()
                    };
                    
                    Expression::New {
                        callee: Box::new(callee),
                        arguments,
                    }
                }
            },
            Some(TokenType::Import) => {
                self.advance(); // consume 'import'
                self.consume(&TokenType::LeftParen, "Expected '(' after 'import'")?;
                let source = self.parse_expression_with_precedence(Precedence::Assignment)?;
                self.consume(&TokenType::RightParen, "Expected ')' after import source")?;
                
                Expression::Import(Box::new(source))
            },
            Some(TokenType::Hash) => {
                self.advance(); // consume '#'
                let name = self.expect_identifier("Expected private identifier name")?;
                Expression::PrivateName(name)
            },
            Some(TokenType::Async) if self.is_async_function() => self.parse_async_function_expression()?,
            _ => {
                let token = self.peek_token().unwrap_or_else(|| self.previous().unwrap());
                return Err(super::error::ParserError::with_token_span(
                    &format!("Unexpected token in expression: {:?}", token.token_type),
                    token.line,
                    token.column,
                    token.length,
                    &self.get_source_text()
                ));
            }
        };

        // Parse infix and postfix expressions based on precedence
        while !self.is_at_end() {
            let current_precedence = match self.peek_token_type() {
                Some(TokenType::Comma) => Precedence::Comma,
                Some(TokenType::Question) => {
                    if self.tokens.get(self.current + 1).map_or(false, |t| matches!(t.token_type, TokenType::Dot)) {
                        Precedence::Call
                    } else {
                        Precedence::Conditional
                    }
                },
                Some(TokenType::Equal) | 
                Some(TokenType::PlusEqual) | 
                Some(TokenType::MinusEqual) |
                Some(TokenType::StarEqual) | 
                Some(TokenType::SlashEqual) | 
                Some(TokenType::PercentEqual) |
                Some(TokenType::StarStarEqual) | 
                Some(TokenType::AmpersandEqual) | 
                Some(TokenType::PipeEqual) |
                Some(TokenType::CaretEqual) | 
                Some(TokenType::LessLessEqual) | 
                Some(TokenType::GreaterGreaterEqual) |
                Some(TokenType::GreaterGreaterGreaterEqual) | 
                Some(TokenType::AmpersandAmpersandEqual) | 
                Some(TokenType::PipePipeEqual) |
                Some(TokenType::QuestionQuestionEqual) => Precedence::Assignment,
                Some(TokenType::PipePipe) | 
                Some(TokenType::QuestionQuestion) => Precedence::LogicalOr,
                Some(TokenType::AmpersandAmpersand) => Precedence::LogicalAnd,
                Some(TokenType::Pipe) => Precedence::BitwiseOr,
                Some(TokenType::Caret) => Precedence::BitwiseXor,
                Some(TokenType::Ampersand) => Precedence::BitwiseAnd,
                Some(TokenType::EqualEqual) | 
                Some(TokenType::BangEqual) | 
                Some(TokenType::EqualEqualEqual) | 
                Some(TokenType::BangEqualEqual) => Precedence::Equality,
                Some(TokenType::Less) | 
                Some(TokenType::LessEqual) | 
                Some(TokenType::Greater) | 
                Some(TokenType::GreaterEqual) |
                Some(TokenType::In) | 
                Some(TokenType::InstanceOf) => Precedence::Relational,
                Some(TokenType::LessLess) | 
                Some(TokenType::GreaterGreater) | 
                Some(TokenType::GreaterGreaterGreater) => Precedence::Shift,
                Some(TokenType::Plus) | 
                Some(TokenType::Minus) => Precedence::Additive,
                Some(TokenType::Star) | 
                Some(TokenType::Slash) | 
                Some(TokenType::Percent) => Precedence::Multiplicative,
                Some(TokenType::StarStar) => Precedence::Exponentiation,
                Some(TokenType::PlusPlus) | 
                Some(TokenType::MinusMinus) if !self.previous_line_terminator() => Precedence::Postfix,
                Some(TokenType::Dot) | 
                Some(TokenType::LeftBracket) | 
                Some(TokenType::LeftParen) |
                Some(TokenType::QuestionDot) => Precedence::Call,
                _ => Precedence::None,
            };

            if precedence > current_precedence {
                break;
            }

            // Handle postfix operators
            if current_precedence == Precedence::Postfix {
                if self.match_any(&[TokenType::PlusPlus, TokenType::MinusMinus]) {
                    if !matches!(expr, Expression::Identifier(_) | Expression::Member { .. }) {
                        let token = self.previous().unwrap();
                        return Err(super::error::ParserError::with_token_span(
                            "Invalid left-hand side in postfix operation", 
                            token.line, 
                            token.column,
                            token.length,
                            &self.get_source_text()
                        ));
                    }
                    
                    let operator = match self.previous().unwrap().token_type {
                        TokenType::PlusPlus => UnaryOperator::Increment,
                        TokenType::MinusMinus => UnaryOperator::Decrement,
                        _ => unreachable!(),
                    };
                    
                    expr = Expression::Unary {
                        operator,
                        argument: Box::new(expr),
                        prefix: false,
                    };
                    continue;
                }
            }

            // Handle infix operators
            match current_precedence {
                Precedence::Comma => {
                    self.advance(); // consume comma
                    let right = self.parse_expression_with_precedence(Precedence::Assignment)?;
                    expr = Expression::Sequence(vec![expr, right]);
                },
                Precedence::Assignment => {
                    // Match assignment operator
                    let op = if self.match_token(&TokenType::Equal) {
                        AssignmentOperator::Assign
                    } else if self.match_token(&TokenType::PlusEqual) {
                        AssignmentOperator::AddAssign
                    } else if self.match_token(&TokenType::MinusEqual) {
                        AssignmentOperator::SubtractAssign
                    } else if self.match_token(&TokenType::StarEqual) {
                        AssignmentOperator::MultiplyAssign
                    } else if self.match_token(&TokenType::SlashEqual) {
                        AssignmentOperator::DivideAssign
                    } else if self.match_token(&TokenType::PercentEqual) {
                        AssignmentOperator::ModuloAssign
                    } else if self.match_token(&TokenType::StarStarEqual) {
                        AssignmentOperator::ExponentAssign
                    } else if self.match_token(&TokenType::AmpersandEqual) {
                        AssignmentOperator::BitwiseAndAssign
                    } else if self.match_token(&TokenType::PipeEqual) {
                        AssignmentOperator::BitwiseOrAssign
                    } else if self.match_token(&TokenType::CaretEqual) {
                        AssignmentOperator::BitwiseXorAssign
                    } else if self.match_token(&TokenType::LessLessEqual) {
                        AssignmentOperator::LeftShiftAssign
                    } else if self.match_token(&TokenType::GreaterGreaterEqual) {
                        AssignmentOperator::RightShiftAssign
                    } else if self.match_token(&TokenType::GreaterGreaterGreaterEqual) {
                        AssignmentOperator::UnsignedRightShiftAssign
                    } else if self.match_token(&TokenType::AmpersandAmpersandEqual) {
                        AssignmentOperator::LogicalAndAssign
                    } else if self.match_token(&TokenType::PipePipeEqual) {
                        AssignmentOperator::LogicalOrAssign
                    } else if self.match_token(&TokenType::QuestionQuestionEqual) {
                        AssignmentOperator::NullishAssign
                    } else {
                        break; // No assignment operator found
                    };
                    
                    // Validate left-hand side
                    if !matches!(expr, Expression::Identifier(_) | Expression::Member { .. } | Expression::Array(_) | Expression::Object(_)) {
                        let binding = Token::new(TokenType::EOF, 0, 0, 0);
                        let token = self.previous().unwrap_or(&binding);
                        return Err(super::error::ParserError::with_token_span(
                            "Invalid left-hand side in assignment", 
                            token.line, 
                            token.column,
                            token.length,
                            &self.get_source_text()
                        ));
                    }
                    
                    let right = self.parse_expression_with_precedence(Precedence::Assignment)?;
                    
                    expr = Expression::Assignment {
                        operator: op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                },
                Precedence::Conditional => {
                    self.advance(); // consume ?
                    
                    // Check if this is part of optional chaining
                    if self.check(&TokenType::Dot) {
                        // This is optional chaining
                        self.advance(); // consume .
                        
                        // Now handle the optional chaining
                        if self.match_token(&TokenType::LeftBracket) {
                            let property = self.parse_expression()?;
                            self.consume(&TokenType::RightBracket, "Expected ']' after computed property")?;
                            expr = Expression::Member {
                                object: Box::new(expr),
                                property: Box::new(property),
                                computed: true,
                                optional: true,
                            };
                        } else if self.match_token(&TokenType::LeftParen) {
                            let arguments = self.parse_arguments()?;
                            expr = Expression::Call {
                                callee: Box::new(expr),
                                arguments,
                                optional: true,
                            };
                        } else {
                            let property = self.expect_identifier("Expected property name 2")?;
                            expr = Expression::Member {
                                object: Box::new(expr),
                                property: Box::new(Expression::Identifier(property)),
                                computed: false,
                                optional: true,
                            };
                        }
                    } else {
                        // This is a ternary operator
                        let consequent = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        self.consume(&TokenType::Colon, "Expected ':' in conditional expression")?;
                        let alternate = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        
                        expr = Expression::Conditional {
                            test: Box::new(expr),
                            consequent: Box::new(consequent),
                            alternate: Box::new(alternate),
                        };
                    }
                },
                Precedence::LogicalOr => {
                    let operator = if self.match_token(&TokenType::PipePipe) {
                        LogicalOperator::Or
                    } else if self.match_token(&TokenType::QuestionQuestion) {
                        LogicalOperator::NullishCoalescing
                    } else {
                        break;
                    };
                    
                    let right = self.parse_expression_with_precedence(Precedence::LogicalAnd)?;
                    
                    expr = Expression::Logical {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                },
                Precedence::LogicalAnd => {
                    self.advance(); // consume &&
                    let right = self.parse_expression_with_precedence(Precedence::BitwiseOr)?;
                    
                    expr = Expression::Logical {
                        operator: LogicalOperator::And,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                },
                Precedence::BitwiseOr | 
                Precedence::BitwiseXor | 
                Precedence::BitwiseAnd | 
                Precedence::Equality | 
                Precedence::Relational | 
                Precedence::Shift | 
                Precedence::Additive | 
                Precedence::Multiplicative => {
                    self.advance();
                    let token_type = self.previous().unwrap().token_type.clone();
                    
                    let operator = match token_type {
                        TokenType::Plus => BinaryOperator::Add,
                        TokenType::Minus => BinaryOperator::Subtract,
                        TokenType::Star => BinaryOperator::Multiply,
                        TokenType::Slash => BinaryOperator::Divide,
                        TokenType::Percent => BinaryOperator::Modulo,
                        TokenType::StarStar => BinaryOperator::Exponent,
                        TokenType::Pipe => BinaryOperator::BitwiseOr,
                        TokenType::Ampersand => BinaryOperator::BitwiseAnd,
                        TokenType::Caret => BinaryOperator::BitwiseXor,
                        TokenType::LessLess => BinaryOperator::LeftShift,
                        TokenType::GreaterGreater => BinaryOperator::RightShift,
                        TokenType::GreaterGreaterGreater => BinaryOperator::UnsignedRightShift,
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        TokenType::BangEqual => BinaryOperator::NotEqual,
                        TokenType::EqualEqualEqual => BinaryOperator::StrictEqual,
                        TokenType::BangEqualEqual => BinaryOperator::StrictNotEqual,
                        TokenType::Less => BinaryOperator::LessThan,
                        TokenType::LessEqual => BinaryOperator::LessThanEqual,
                        TokenType::Greater => BinaryOperator::GreaterThan,
                        TokenType::GreaterEqual => BinaryOperator::GreaterThanEqual,
                        TokenType::In => BinaryOperator::In,
                        TokenType::InstanceOf => BinaryOperator::InstanceOf,
                        _ => {
                            let token = self.previous().unwrap();
                            return Err(super::error::ParserError::with_token_span(
                                &format!("Unexpected token: {:?}", token_type),
                                token.line,
                                token.column,
                                token.length,
                                &self.get_source_text()
                            ));
                        }
                    };
                    
                    // Determine next precedence level
                    let next_precedence = match current_precedence {
                        Precedence::BitwiseOr => Precedence::BitwiseXor,
                        Precedence::BitwiseXor => Precedence::BitwiseAnd,
                        Precedence::BitwiseAnd => Precedence::Equality,
                        Precedence::Equality => Precedence::Relational,
                        Precedence::Relational => Precedence::Shift,
                        Precedence::Shift => Precedence::Additive,
                        Precedence::Additive => Precedence::Multiplicative,
                        Precedence::Multiplicative => Precedence::Exponentiation,
                        _ => unreachable!(),
                    };
                    
                    let right = self.parse_expression_with_precedence(next_precedence)?;
                    
                    expr = Expression::Binary {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                },
                Precedence::Exponentiation => {
                    self.advance(); // consume **
                    // Exponentiation is right-associative
                    let right = self.parse_expression_with_precedence(Precedence::Exponentiation)?;
                    
                    expr = Expression::Binary {
                        operator: BinaryOperator::Exponent,
                        left: Box::new(expr),
                        right: Box::new(right),
                    };
                },
                Precedence::Call => {
                    if self.match_token(&TokenType::Dot) {
                        let property = if let Some(TokenType::Identifier(name)) = self.peek_token_type().cloned() {
                            self.advance();
                            name.into_boxed_str()
                        }

                        else if self.check(&TokenType::Default) {
                            self.advance();
                            "default".into()
                        } else if self.check(&TokenType::Get) {
                            self.advance();
                            "get".into()
                        } else if self.check(&TokenType::Set) {
                            self.advance();
                            "set".into()
                        } else if self.check(&TokenType::From) {
                            self.advance();
                            "from".into()
                        } else if self.check(&TokenType::As) {
                            self.advance();
                            "as".into()
                        } else if self.check(&TokenType::For) {
                            self.advance();
                            "for".into()
                        } else {
                            return Err(super::error::ParserError::with_token_span(
                                "Expected property name 3",
                                self.peek_token().unwrap().line,
                                self.peek_token().unwrap().column,
                                self.peek_token().unwrap().length,
                                &self.get_source_text()
                            ));
                        };
                        
                        expr = Expression::Member {
                            object: Box::new(expr),
                            property: Box::new(Expression::Identifier(property)),
                            computed: false,
                            optional: false,
                        };
                    } else if self.match_token(&TokenType::LeftBracket) {
                        // Member access with bracket notation
                        let property = self.parse_expression()?;
                        self.consume(&TokenType::RightBracket, "Expected ']' after computed property")?;
                        expr = Expression::Member {
                            object: Box::new(expr),
                            property: Box::new(property),
                            computed: true,
                            optional: false,
                        };
                    } else if self.match_token(&TokenType::QuestionDot) {
                        // Optional chaining
                        if self.match_token(&TokenType::LeftBracket) {
                            let property = self.parse_expression()?;
                            self.consume(&TokenType::RightBracket, "Expected ']' after computed property")?;
                            expr = Expression::Member {
                                object: Box::new(expr),
                                property: Box::new(property),
                                computed: true,
                                optional: true,
                            };
                        } else {
                            let property = self.expect_identifier("Expected property name 4")?;
                            expr = Expression::Member {
                                object: Box::new(expr),
                                property: Box::new(Expression::Identifier(property)),
                                computed: false,
                                optional: true,
                            };
                        }
                    } else if self.match_token(&TokenType::LeftParen) {
                        // Function call
                        let mut args = Vec::new();
                        
                        if !self.check(&TokenType::RightParen) {
                            loop {
                                if self.match_token(&TokenType::Ellipsis) {
                                    // Spread argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Spread(expr));
                                } else {
                                    // Regular argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Expression(expr));
                                }
                                
                                if !self.match_token(&TokenType::Comma) {
                                    break;
                                }
                                
                                // Handle trailing comma
                                if self.check(&TokenType::RightParen) {
                                    break;
                                }
                            }
                        }
                        
                        self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
                        
                        expr = Expression::Call {
                            callee: Box::new(expr),
                            arguments: args,
                            optional: false,
                        };
                    } else {
                        break;
                    }
                },
                _ => break,
            }
        }

        Ok(expr)
    }

}
