use super::prelude::*;

use crate::ast::*;
use crate::lexer::{Token, TemplatePart, LexicalContext};
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
        self.parse_expression_with_precedence(Precedence::Comma)
    }

    pub fn parse_expression_with_precedence(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        // Parse prefix expressions
        let mut expr = match self.peek() {
            // Unary prefix operators
            Some(Token::Bang) | 
            Some(Token::Tilde) | 
            Some(Token::Plus) | 
            Some(Token::Minus) |
            Some(Token::PlusPlus) | 
            Some(Token::MinusMinus) | 
            Some(Token::Typeof) |
            Some(Token::Void) | 
            Some(Token::Delete) => {
                self.advance();
                let operator = match self.peek_previous().unwrap() {
                    Token::Bang => UnaryOperator::Not,
                    Token::Tilde => UnaryOperator::BitwiseNot,
                    Token::Plus => UnaryOperator::Plus,
                    Token::Minus => UnaryOperator::Minus,
                    Token::PlusPlus => UnaryOperator::Increment,
                    Token::MinusMinus => UnaryOperator::Decrement,
                    Token::Typeof => UnaryOperator::Typeof,
                    Token::Void => UnaryOperator::Void,
                    Token::Delete => UnaryOperator::Delete,
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
            Some(Token::Await) if self.allows_await() => {
                self.advance();
                let argument = self.parse_expression_with_precedence(Precedence::Prefix)?;
                Expression::Await(Box::new(argument))
            },
            // Yield expression
            Some(Token::Yield) if self.allows_yield() => {
                self.advance();
                let delegate = self.match_token(&Token::Star);
                
                // Yield can be used without an argument
                let argument = if self.check(&Token::Semicolon) || 
                                self.check(&Token::RightBrace) || 
                                self.check(&Token::Comma) ||
                                self.check(&Token::RightParen) ||
                                self.check(&Token::Colon) ||
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
            Some(Token::This) => {
                self.advance();
                Expression::This
            },
            Some(Token::Arguments) => {
                self.advance();
                Expression::Identifier("arguments".into())
            },
            Some(Token::Super) => {
                self.advance();
                Expression::Super
            },
            Some(Token::Null) => {
                self.advance();
                Expression::Literal(Literal::Null)
            },
            Some(Token::Undefined) => {
                self.advance();
                Expression::Literal(Literal::Undefined)
            },
            Some(Token::True) => {
                self.advance();
                Expression::Literal(Literal::Boolean(true))
            },
            Some(Token::False) => {
                self.advance();
                Expression::Literal(Literal::Boolean(false))
            },
            Some(Token::NumberLiteral(n)) => {
                let value = *n;
                self.advance();
                Expression::Literal(Literal::Number(value))
            },
            Some(Token::StringLiteral(_)) => {
                if let Token::StringLiteral(s) = &self.advance().unwrap() {
                    Expression::Literal(Literal::String(s.clone().into_boxed_str()))
                } else {
                    unreachable!()
                }
            },
            Some(Token::RegExpLiteral(_, _)) => {
                if let Token::RegExpLiteral(pattern, flags) = self.advance().unwrap().clone() {
                    Expression::Literal(Literal::RegExp {
                        pattern: pattern.into_boxed_str(),
                        flags: flags.into_boxed_str(),
                    })
                } else {
                    unreachable!()
                }
            },
            Some(Token::BigIntLiteral(_)) => {
                if let Token::BigIntLiteral(s) = self.advance().unwrap().clone() {
                    Expression::Literal(Literal::BigInt(s.into_boxed_str()))
                } else {
                    unreachable!()
                }
            },
            Some(Token::TemplateLiteral(_)) => {
                if let Token::TemplateLiteral(parts) = self.advance().unwrap().clone() {
                    
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
                                                return Err(parser_error_at_current!(self, "Invalid expression in template literal: {}", e.message));
                                            }
                                        }
                                    },
                                    Err(e) => {
                                        return Err(parser_error_at_current!(self, "Error tokenizing expression in template literal: {}", e.message));
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
                            return Err(parser_error_at_current!(self, "Invalid template literal: expected {} quasis but got {}", expressions.len() + 1, quasis.len()));
                        }
                    }
                    
                    Expression::TemplateLiteral { quasis, expressions }
                } else {
                    unreachable!("Expected TemplateLiteral token")
                }
            },
            // TODO everything but Identifier hoists matches below, need a better approach to var as = e.class; scenarios
            Some(Token::Identifier(_)) => {
                let name = self.expect_identifier("Expected identifier in expression")?;
                if self.check(&Token::Arrow) {
                    let param = Expression::Identifier(name);
                    self.advance();
                    return self.parse_arrow_function_body(vec![param], false);
                }
                Expression::Identifier(name)
            },
            Some(Token::LeftParen) => {
                //println!("In ( {:#?}", self.peek());
                self.advance(); // Consume the '('

                // Handle empty parameter list: () => ...
                if self.match_token(&Token::RightParen) {
                    return if self.match_token(&Token::Arrow) {
                        self.parse_arrow_function_body(vec![], false)
                    } else {
                        Err(parser_error_at_current!(self, "Unexpected empty parentheses '()'"))
                    };
                }

                //println!("Here 1 current token {:#?}", self.peek());
                let mut expr = self.parse_expression()?;

                //println!("Here 2");
                // Handle single-parameter or nested parentheses: (x) => ..., ((expr))
                if self.match_token(&Token::RightParen) {
                    if self.match_token(&Token::Arrow) {
                        let params = match expr {
                            //Expression::Identifier(_) => vec![expr],
                            //Expression::Sequence(seq) => seq,
                            Expression::Sequence(seq) => seq,//self.flatten_sequence(seq),
                            _ => vec![expr],
                        };
                        return self.parse_arrow_function_body(params, false);
                    }
                } else if self.check(&Token::Comma) {

                    //println!("Some comma {:#?}", self.peek());

                    // Handle comma-separated parameters: (a, b, c)
                    let mut params = vec![expr];
                    while self.match_token(&Token::Comma) {
                        //println!("Current token {:#?}", self.peek());
                        params.push(self.parse_expression_with_precedence(Precedence::Assignment)?);
                    }
                    self.consume(&Token::RightParen, "Expected ')' after parameters")?;
                    return if self.match_token(&Token::Arrow) {
                        self.parse_arrow_function_body(params, false)
                    } else {
                        Ok(Expression::Sequence(params))
                    };
                } else {
                    self.consume(&Token::RightParen, "Expected ')' after expression")?;
                }

                // Handle expressions after ')': ., [ or (
                if self.match_token(&Token::Dot) {
                    expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                } else if self.check(&Token::LeftBracket) || self.check(&Token::LeftParen) {
                    expr = self.parse_expression_with_precedence(Precedence::Call)?;
                }

                expr
            },
            Some(Token::LeftBracket) => {
                //println!("I am here");
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&Token::RightBracket) && !self.is_at_end() {
                    if self.match_token(&Token::Comma) {
                        elements.push(ArrayElement::Hole);
                    } else {
                        if self.match_token(&Token::Ellipsis) {
                            let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                            elements.push(ArrayElement::Spread(expr));
                        } else {
                            let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                            elements.push(ArrayElement::Expression(expr));
                        }
                        if !self.check(&Token::RightBracket) {
                            self.consume(&Token::Comma, "Expected ',' after array element")?;
                        }
                    }
                }
                self.consume(&Token::RightBracket, "Expected ']' after array elements")?;
                Expression::Array(elements)
            },
            Some(Token::LeftBrace) => {
                self.advance();
                let mut properties = Vec::new();
                while !self.check(&Token::RightBrace) && !self.is_at_end() {
                    if self.match_token(&Token::Ellipsis) {
                        let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        properties.push(ObjectProperty::Spread(expr));
                    } else {
                        let is_async = self.match_token(&Token::Async);
                        let is_generator = self.match_token(&Token::Star);
                        let mut kind = PropertyKind::Init;
                        if !is_async && !is_generator {
                            if self.check(&Token::Get) || self.check(&Token::Set) {
                                let is_property_name = if let Some(next_token) = self.peek_next(1) {
                                    matches!(next_token, Token::Colon)
                                } else {
                                    false
                                };
                                if !is_property_name {
                                    if self.match_token(&Token::Get) {
                                        kind = PropertyKind::Get;
                                    } else if self.match_token(&Token::Set) {
                                        kind = PropertyKind::Set;
                                    }
                                }
                            }
                        }

                        // Use with_context for property key parsing
                        let key = self.with_context(LexicalContext::PropertyKey, |parser| {
                            parser.parse_property_key()
                        })?;
                        
                        let computed = matches!(key, PropertyKey::Computed(_));
                        
                        // Method definition
                        if self.check(&Token::LeftParen) || is_generator || is_async {
                            let method_kind = match kind {
                                PropertyKind::Get => MethodKind::Getter,
                                PropertyKind::Set => MethodKind::Setter,
                                _ => MethodKind::Method,
                            };
                            
                            let params = self.parse_function_params()?;
                            
                            self.consume(&Token::LeftBrace, "Expected '{' before function body")?;
                            let body = self.parse_function_body(is_async, is_generator)?;
                            self.consume(&Token::RightBrace, "Expected '}' after function body")?;
                            
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
                                            !self.check(&Token::Colon) && 
                                            matches!(key, PropertyKey::Identifier(_));
                            
                            let value = if shorthand {
                                if let PropertyKey::Identifier(name) = &key {
                                    Expression::Identifier(name.clone())
                                } else {
                                    unreachable!()
                                }
                            } else {
                                self.consume(&Token::Colon, "Expected ':' after property name")?;
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
                    
                    if !self.check(&Token::RightBrace) {
                        //println!("Now have token {:#?}", self.peek());

                        self.consume(&Token::Comma, "Expected ',' after property")?;
                        
                        // Allow trailing comma
                        if self.check(&Token::RightBrace) {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                self.consume(&Token::RightBrace, "Expected '}' after object literal")?;

                Expression::Object(properties)
            },

            Some(Token::Function) => self.parse_function_expression()?,
            Some(Token::Class) => self.parse_class_expression()?,
            Some(Token::New) => {
                self.advance(); // consume 'new'
                
                // Handle new.target meta property
                if self.match_token(&Token::Dot) {
                    if let Some(Token::Identifier(name)) = self.peek().cloned() {
                        if name == "target" {
                            self.advance(); // consume 'target'
                            Expression::MetaProperty {
                                meta: "new".into(),
                                property: "target".into(),
                            }
                        } else {
                            return Err(parser_error_at_current!(self, "Expected 'target' after 'new.'"));
                        }
                    } else {
                        return Err(parser_error_at_current!(self, "Expected 'target' after 'new.'"));
                    }
                } else {
                    // Regular new expression
                    let callee = self.parse_expression_with_precedence(Precedence::Call)?;
                    
                    Expression::New(Box::new(callee))
                }
            },
            Some(Token::Import) => {
                self.advance(); // consume 'import'
                self.consume(&Token::LeftParen, "Expected '(' after 'import'")?;
                let source = self.parse_expression_with_precedence(Precedence::Assignment)?;
                self.consume(&Token::RightParen, "Expected ')' after import source")?;
                
                Expression::Import(Box::new(source))
            },
            Some(Token::Hash) => {
                self.advance(); // consume '#'
                let name = self.expect_identifier("Expected private identifier name")?;
                Expression::PrivateName(name)
            },
            Some(Token::Async) if self.is_async_function() => self.parse_async_function_expression()?,
            _ => {
                return Err(parser_error_at_current!(self, "Unexpected token in expression"));
            }
        };

        // Parse infix and postfix expressions based on precedence
        while !self.is_at_end() {
            let current_precedence = match self.peek() {
                Some(Token::Comma) => Precedence::Comma,
                Some(Token::Question) => {
                    match self.peek_next(1) {
                        Some(Token::Dot) => Precedence::Call,
                        _ => Precedence::Conditional,
                    }
                },
                Some(Token::Equal) | 
                Some(Token::PlusEqual) | 
                Some(Token::MinusEqual) |
                Some(Token::StarEqual) | 
                Some(Token::SlashEqual) | 
                Some(Token::PercentEqual) |
                Some(Token::StarStarEqual) | 
                Some(Token::AmpersandEqual) | 
                Some(Token::PipeEqual) |
                Some(Token::CaretEqual) | 
                Some(Token::LessLessEqual) | 
                Some(Token::GreaterGreaterEqual) |
                Some(Token::GreaterGreaterGreaterEqual) | 
                Some(Token::AmpersandAmpersandEqual) | 
                Some(Token::PipePipeEqual) |
                Some(Token::QuestionQuestionEqual) => Precedence::Assignment,
                Some(Token::PipePipe) | 
                Some(Token::QuestionQuestion) => Precedence::LogicalOr,
                Some(Token::AmpersandAmpersand) => Precedence::LogicalAnd,
                Some(Token::Pipe) => Precedence::BitwiseOr,
                Some(Token::Caret) => Precedence::BitwiseXor,
                Some(Token::Ampersand) => Precedence::BitwiseAnd,
                Some(Token::EqualEqual) | 
                Some(Token::BangEqual) | 
                Some(Token::EqualEqualEqual) | 
                Some(Token::BangEqualEqual) => Precedence::Equality,
                Some(Token::Less) | 
                Some(Token::LessEqual) | 
                Some(Token::Greater) | 
                Some(Token::GreaterEqual) |
                Some(Token::In) | 
                Some(Token::InstanceOf) => Precedence::Relational,
                Some(Token::LessLess) | 
                Some(Token::GreaterGreater) | 
                Some(Token::GreaterGreaterGreater) => Precedence::Shift,
                Some(Token::Plus) | 
                Some(Token::Minus) => Precedence::Additive,
                Some(Token::Star) | 
                Some(Token::Slash) | 
                Some(Token::Percent) => Precedence::Multiplicative,
                Some(Token::StarStar) => Precedence::Exponentiation,
                Some(Token::PlusPlus) | 
                Some(Token::MinusMinus) => Precedence::Postfix,
                Some(Token::Dot) | 
                Some(Token::LeftBracket) | 
                Some(Token::LeftParen) |
                Some(Token::QuestionDot) => Precedence::Call,
                _ => Precedence::None,
            };

            if current_precedence == Precedence::None || precedence > current_precedence {
                break;
            }
            // Handle postfix operators
            if current_precedence == Precedence::Postfix {
                if self.match_any(&[Token::PlusPlus, Token::MinusMinus]) {
                    if !matches!(expr, Expression::Identifier(_) | Expression::Member { .. }) {
                        return Err(parser_error_at_current!(self, "Invalid left-hand side in postfix operation"));
                    }
                    
                    let operator = match self.peek_previous().unwrap() {
                        Token::PlusPlus => UnaryOperator::Increment,
                        Token::MinusMinus => UnaryOperator::Decrement,
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

                    if !self.check(&Token::RightParen) {                    
                        let right = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        if let Expression::Sequence(ref mut seq) = expr {
                            seq.push(right);
                        } else {
                            expr = Expression::Sequence(vec![expr, right]);
                        }
                    }
                },
                Precedence::Assignment => {
                    // Match assignment operator
                    let op = if self.match_token(&Token::Equal) {
                        AssignmentOperator::Assign
                    } else if self.match_token(&Token::PlusEqual) {
                        AssignmentOperator::AddAssign
                    } else if self.match_token(&Token::MinusEqual) {
                        AssignmentOperator::SubtractAssign
                    } else if self.match_token(&Token::StarEqual) {
                        AssignmentOperator::MultiplyAssign
                    } else if self.match_token(&Token::SlashEqual) {
                        AssignmentOperator::DivideAssign
                    } else if self.match_token(&Token::PercentEqual) {
                        AssignmentOperator::ModuloAssign
                    } else if self.match_token(&Token::StarStarEqual) {
                        AssignmentOperator::ExponentAssign
                    } else if self.match_token(&Token::AmpersandEqual) {
                        AssignmentOperator::BitwiseAndAssign
                    } else if self.match_token(&Token::PipeEqual) {
                        AssignmentOperator::BitwiseOrAssign
                    } else if self.match_token(&Token::CaretEqual) {
                        AssignmentOperator::BitwiseXorAssign
                    } else if self.match_token(&Token::LessLessEqual) {
                        AssignmentOperator::LeftShiftAssign
                    } else if self.match_token(&Token::GreaterGreaterEqual) {
                        AssignmentOperator::RightShiftAssign
                    } else if self.match_token(&Token::GreaterGreaterGreaterEqual) {
                        AssignmentOperator::UnsignedRightShiftAssign
                    } else if self.match_token(&Token::AmpersandAmpersandEqual) {
                        AssignmentOperator::LogicalAndAssign
                    } else if self.match_token(&Token::PipePipeEqual) {
                        AssignmentOperator::LogicalOrAssign
                    } else if self.match_token(&Token::QuestionQuestionEqual) {
                        AssignmentOperator::NullishAssign
                    } else {
                        break; // No assignment operator found
                    };
                    
                    // Validate left-hand side
                    if !matches!(expr, Expression::Identifier(_) | Expression::Member { .. } | Expression::Array(_) | Expression::Object(_)) {
                        return Err(parser_error_at_current!(self, "Invalid left-hand side in assignment"));
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
                    if self.check(&Token::Dot) {
                        // This is optional chaining
                        self.advance(); // consume .
                        
                        // Now handle the optional chaining
                        if self.match_token(&Token::LeftBracket) {
                            let property = self.parse_expression()?;
                            self.consume(&Token::RightBracket, "Expected ']' after computed property")?;
                            expr = Expression::Member {
                                object: Box::new(expr),
                                property: Box::new(property),
                                computed: true,
                                optional: true,
                            };
                        } else if self.match_token(&Token::LeftParen) {
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
                        self.consume(&Token::Colon, "Expected ':' in conditional expression")?;
                        let alternate = self.parse_expression_with_precedence(Precedence::Assignment)?;
                        
                        expr = Expression::Conditional {
                            test: Box::new(expr),
                            consequent: Box::new(consequent),
                            alternate: Box::new(alternate),
                        };
                    }
                },
                Precedence::LogicalOr => {
                    let operator = if self.match_token(&Token::PipePipe) {
                        LogicalOperator::Or
                    } else if self.match_token(&Token::QuestionQuestion) {
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
                // TODO implement
//                Precedence::NullishCoalescing => {  
//                    self.advance(); // consume '??'  
//                    let right = self.parse_expression_with_precedence(Precedence::NullishCoalescing)?;  
                      //
//                    expr = Expression::Logical {  
//                        operator: LogicalOperator::NullishCoalescing,  
//                        left: Box::new(expr),  
//                        right: Box::new(right),  
//                    };  
//                },
                Precedence::BitwiseOr | 
                Precedence::BitwiseXor | 
                Precedence::BitwiseAnd | 
                Precedence::Equality | 
                Precedence::Relational | 
                Precedence::Shift | 
                Precedence::Additive | 
                Precedence::Multiplicative => {
                    self.advance();
                    let token_type = self.peek_previous().unwrap().clone();
                    
                    let operator = match token_type {
                        Token::Plus => BinaryOperator::Add,
                        Token::Minus => BinaryOperator::Subtract,
                        Token::Star => BinaryOperator::Multiply,
                        Token::Slash => BinaryOperator::Divide,
                        Token::Percent => BinaryOperator::Modulo,
                        Token::StarStar => BinaryOperator::Exponent,
                        Token::Pipe => BinaryOperator::BitwiseOr,
                        Token::Ampersand => BinaryOperator::BitwiseAnd,
                        Token::Caret => BinaryOperator::BitwiseXor,
                        Token::LessLess => BinaryOperator::LeftShift,
                        Token::GreaterGreater => BinaryOperator::RightShift,
                        Token::GreaterGreaterGreater => BinaryOperator::UnsignedRightShift,
                        Token::EqualEqual => BinaryOperator::Equal,
                        Token::BangEqual => BinaryOperator::NotEqual,
                        Token::EqualEqualEqual => BinaryOperator::StrictEqual,
                        Token::BangEqualEqual => BinaryOperator::StrictNotEqual,
                        Token::Less => BinaryOperator::LessThan,
                        Token::LessEqual => BinaryOperator::LessThanEqual,
                        Token::Greater => BinaryOperator::GreaterThan,
                        Token::GreaterEqual => BinaryOperator::GreaterThanEqual,
                        Token::In => BinaryOperator::In,
                        Token::InstanceOf => BinaryOperator::InstanceOf,
                        _ => {
                            return Err(parser_error_at_current!(self, "Unexpected token: {:?}", token_type));
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

                    //println!("In call {:#?}", self.peek());

                    if self.match_token(&Token::Dot) {

                        let property = self.with_context(LexicalContext::MemberAccess, |parser| {
                            if let Some(Token::Identifier(name)) = parser.peek().cloned() {
                                parser.advance();
                                return Ok(name.into_boxed_str())
                            } else {
                                return Err(parser_error_at_current!(parser, "Expected property name 3"));
                            };
                        })?;
                        
                        expr = Expression::Member {
                            object: Box::new(expr),
                            property: Box::new(Expression::Identifier(property)),
                            computed: false,
                            optional: false,
                        };
                    } else if self.match_token(&Token::LeftBracket) {
                        //println!("This case");
                        // Member access with bracket notation
                        let property = self.parse_expression()?;
                        self.consume(&Token::RightBracket, "Expected ']' after computed property")?;
                        expr = Expression::Member {
                            object: Box::new(expr),
                            property: Box::new(property),
                            computed: true,
                            optional: false,
                        };
                    } else if self.match_token(&Token::QuestionDot) {
                        // Optional chaining
                        if self.match_token(&Token::LeftBracket) {
                            let property = self.parse_expression()?;
                            self.consume(&Token::RightBracket, "Expected ']' after computed property")?;
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
                    } else if self.match_token(&Token::LeftParen) {

                        // Function call
                        let mut args = Vec::new();
                        
                        if !self.check(&Token::RightParen) {
                            loop {
                                if self.match_token(&Token::Ellipsis) {
                                    // Spread argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Spread(expr));
                                } else {
                                    // Regular argument
                                    let expr = self.parse_expression_with_precedence(Precedence::Assignment)?;
                                    args.push(Argument::Expression(expr));
                                }
                                
                                if !self.match_token(&Token::Comma) {
                                    break;
                                }
                                
                                // Handle trailing comma
                                if self.check(&Token::RightParen) {
                                    break;
                                }
                            }
                        }
                        
                        self.consume(&Token::RightParen, "Expected ')' after arguments 2")?;
                        
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
