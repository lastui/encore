use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::function::*;
use super::class::*;
use super::array::*;
use super::pattern::*;
use super::literal::*;
use super::object::*;
use super::this::*;
use super::new::*;
use super::await_expression::*;
use super::yield_expression::*;


/// Parser for JavaScript expressions
pub struct ExpressionNode;

impl ExpressionNode {
    pub fn new() -> Self {
        Self
    }
    
    /// Parse an expression with the given precedence level
    pub fn parse_with_precedence(&self, parser: &mut Parser, precedence: Precedence) -> ParseResult<Expression> {
        // Parse the prefix expression
        let mut left = self.parse_prefix(parser)?;
        
        // Continue parsing infix expressions as long as they have higher precedence
        while !parser.is_at_end() && precedence < self.get_precedence(parser) {
            left = self.parse_infix(parser, left)?;
        }

        Ok(left)
    }
    
    /// Parse a prefix expression
    fn parse_prefix(&self, parser: &mut Parser) -> ParseResult<Expression> {
        match parser.peek() {
            Token::Identifier(_) => {
                // Check if this is a single-parameter arrow function
                let pos = parser.save_position();
                let ident = IdentifierNode::new().parse(parser)?;
                
                if parser.check(&Token::Arrow) {
                    // This is an arrow function with a single parameter
                    parser.restore_position(pos);
                    return ArrowFunctionExpressionNode::new().parse(parser).map(Expression::ArrowFunctionExpression);
                }
                
                Ok(Expression::Identifier(ident))
            },
            Token::StringLiteral(_) | 
            Token::NumberLiteral(_) | 
            Token::BigIntLiteral(_) |
            Token::RegExpLiteral(_, _) |
            Token::True |
            Token::False |
            Token::Undefined |
            Token::Null => {
                LiteralNode::new().parse(parser).map(Expression::Literal)
            },
            Token::This => {
                ThisExpressionNode::new().parse(parser).map(Expression::ThisExpression)
            },
            Token::LeftBracket => {
                ArrayExpressionNode::new().parse(parser).map(Expression::ArrayExpression)
            },
            Token::LeftBrace => {
                ObjectExpressionNode::new().parse(parser).map(Expression::ObjectExpression)
            },
            Token::Function => {
                FunctionExpressionNode::new().parse(parser).map(Expression::FunctionExpression)
            },
            Token::Class => {
                ClassExpressionNode::new().parse(parser).map(Expression::ClassExpression)
            },
            Token::New => {
                NewExpressionNode::new().parse(parser).map(Expression::NewExpression)
            },
            Token::Super => {
                SuperExpressionNode::new().parse(parser).map(Expression::SuperExpression)
            },
            Token::LeftParen => {
                self.parse_grouped_expression(parser)
            },
            Token::PlusPlus |
            Token::MinusMinus => {
                self.parse_prefix_update_expression(parser)
            },
            Token::Plus |
            Token::Minus |
            Token::Bang | 
            Token::Tilde |
            Token::Typeof |
            Token::Void | 
            Token::Delete => {
                self.parse_unary_expression(parser)
            },
            Token::Await => {
                if parser.allows_await() {
                    AwaitExpressionNode::new().parse(parser).map(Expression::AwaitExpression)
                } else {
                    Err(parser.error_at_current("'await' expression is only allowed within async functions"))
                }
            },
            Token::Yield => {
                if parser.allows_yield() {
                    YieldExpressionNode::new().parse(parser).map(Expression::YieldExpression)
                } else {
                    Err(parser.error_at_current("'yield' expression is only allowed within generator functions"))
                }
            },
            Token::Async => {
                // Save position to potentially backtrack
                let pos = parser.save_position();
                
                // Check if this is an async function
                if parser.peek_next(1) == &Token::Function {
                    // This is an async function expression
                    parser.advance(); // Consume 'async'
                    
                    // Parse the function expression
                    let mut func_expr = FunctionExpressionNode::new().parse(parser)?;
                    func_expr.async_function = true; // Mark as async
                    
                    Ok(Expression::FunctionExpression(func_expr))
                } 
                // Check if this is an async arrow function
                else if parser.peek_next(1) == &Token::LeftParen || 
                         (matches!(parser.peek_next(1), Token::Identifier(_)) && 
                          parser.peek_next(2) == &Token::Arrow) {
                    // This is an async arrow function
                    ArrowFunctionExpressionNode::new().parse(parser).map(Expression::ArrowFunctionExpression)
                } 
                // Otherwise, it's just an identifier named "async"
                else {
                    IdentifierNode::new().parse(parser).map(Expression::Identifier)
                }
            },
            _ => Err(parser.error_at_current("Expected an expression")),
        }
    }
    
    /// Parse an infix expression
    fn parse_infix(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        match parser.peek() {
            Token::LeftParen => {
                self.parse_with_callee(parser, left, false).map(Expression::CallExpression)
            },
            Token::LeftBracket |
            Token::Dot => {
                self.parse_with_object(parser, left, false).map(Expression::MemberExpression)
            },
            Token::QuestionDot => {
                if matches!(parser.peek_next(1), &Token::LeftParen) {
                    self.parse_with_callee(parser, left, true).map(Expression::CallExpression)
                } else {
                    self.parse_with_object(parser, left, true).map(Expression::MemberExpression)
                }
            },
            Token::PlusPlus |
            Token::MinusMinus => {
                self.parse_postfix_update_expression(parser, left)
            },
            Token::Plus |
            Token::Minus |
            Token::Star | 
            Token::Slash |
            Token::Percent |
            Token::StarStar |
            Token::LessLess |
            Token::GreaterGreater | 
            Token::GreaterGreaterGreater |
            Token::Ampersand |
            Token::Pipe |
            Token::Caret |
            Token::EqualEqual |
            Token::BangEqual |
            Token::EqualEqualEqual | 
            Token::BangEqualEqual |
            Token::Less |
            Token::LessEqual |
            Token::Greater |
            Token::GreaterEqual |
            Token::In |
            Token::InstanceOf => {
                self.parse_binary_expression(parser, left)
            },
            Token::AmpersandAmpersand |
            Token::PipePipe | 
            Token::QuestionQuestion => {
                self.parse_logical_expression(parser, left)
            },
            Token::Question => {
                self.parse_conditional_expression(parser, left)
            },
            Token::Equal |
            Token::PlusEqual |
            Token::MinusEqual |
            Token::StarEqual |
            Token::SlashEqual |
            Token::PercentEqual |
            Token::StarStarEqual |
            Token::LessLessEqual | 
            Token::GreaterGreaterEqual |
            Token::GreaterGreaterGreaterEqual |
            Token::AmpersandEqual |
            Token::PipeEqual |
            Token::CaretEqual |
            Token::AmpersandAmpersandEqual |
            Token::PipePipeEqual |
            Token::QuestionQuestionEqual => {
                self.parse_assignment_expression(parser, left)
            },
            Token::Comma => {
                self.parse_sequence_expression(parser, left)
            },
            Token::Arrow => {
                // This should be handled by the arrow function parser
                Err(parser.error_at_current("Unexpected arrow function"))
            },
            _ => Ok(left),
        }
    }

    fn parse_grouped_expression(&self, parser: &mut Parser) -> ParseResult<Expression> {
        // Save position in case we need to backtrack for arrow functions
        let pos = parser.save_position();
        
        // Check if this might be an arrow function with parameters
        if self.is_arrow_function_ahead(parser) {
            parser.restore_position(pos);
            return ArrowFunctionExpressionNode::new().parse(parser).map(Expression::ArrowFunctionExpression);
        }
        
        // Consume the opening parenthesis
        parser.assert_consume(&Token::LeftParen, "Expected '(' at the start of grouped expression")?;
        
        // Check for empty parentheses
        if parser.consume(&Token::RightParen) {
            return Err(parser.error_at_current("Empty parentheses are not a valid expression"));
        }
        
        // Parse the expression inside the parentheses
        let expr = self.parse_with_precedence(parser, Precedence::Lowest)?;
        
        // Check for trailing comma (which is not allowed in grouped expressions)
        if parser.consume(&Token::Comma) {
            return Err(parser.error_at_current("Unexpected trailing comma in grouped expression"));
        }
        
        // Consume the closing parenthesis
        parser.assert_consume(&Token::RightParen, "Expected ')' after expression")?;
        
        Ok(expr)
    }

    fn parse_with_callee(&self, parser: &mut Parser, callee: Expression, optional: bool) -> ParseResult<CallExpression> {
        // Consume the question-dot token if this is optional chaining
        if optional {
            parser.assert_consume(&Token::QuestionDot, "Expected '?.' in optional chaining")?;
        }
        
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut arguments = Vec::new();

        if !parser.check(&Token::RightParen) {
            arguments.push(ExpressionNode::new().parse(parser)?);

            while parser.consume(&Token::Comma) && !parser.check(&Token::RightParen) {
                arguments.push(ExpressionNode::new().parse(parser)?);
            }
        }

        parser.assert_consume(&Token::RightParen, "Expected ')' after function arguments")?;
        
        Ok(CallExpression {
            callee: Box::new(callee),
            arguments,
            optional,
        })
    }

    fn parse_with_object(&self, parser: &mut Parser, object: Expression, optional: bool) -> ParseResult<MemberExpression> {
        // Consume the dot or question-dot token
        if optional {
            parser.assert_consume(&Token::QuestionDot, "Expected '?.' in optional chaining")?;
        } else if parser.check(&Token::Dot) {
            parser.advance(); // Consume the '.'
        }

        // Parse the property access
        let (property, computed) = if parser.consume(&Token::LeftBracket) {
            // Computed property access: obj[expr] or obj?.[expr]
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property")?;
            (MemberProperty::Expression(Box::new(expr)), true)
        } else {
            // Static property access: obj.prop or obj?.prop
            if let Token::Identifier(_) = parser.peek() {
                let ident = IdentifierNode::new().parse(parser)?;
                (MemberProperty::Identifier(ident), false)
            } else if let Token::Default = parser.peek() {
                // Special case for 'default' as property name
                parser.advance(); // Consume the 'default' token
                let name = "default".to_string().into_boxed_str();
                (MemberProperty::Identifier(Identifier { name }), false)
            } else {
                return Err(parser.error_at_current("Expected identifier after '.' or '?.'"));
            }
        };

        // Create the member expression
        let member_expr = MemberExpression {
            object: Box::new(object),
            property,
            computed,
            optional,
        };

        if parser.check(&Token::LeftBracket) || parser.check(&Token::Dot) {
            // Continue parsing the chain of regular property accesses
            return self.parse_with_object(parser, Expression::MemberExpression(member_expr), false);
        } else if parser.check(&Token::QuestionDot) {
            // Save position to check what follows
            let pos = parser.save_position();
            parser.advance(); // Consume '?.'
            
            if parser.check(&Token::LeftParen) {
                // This would be a function call, which we can't handle here
                // Restore position and return the member expression we've parsed so far
                parser.restore_position(pos);
                return Ok(member_expr);
            } else {
                // Continue with optional property access
                parser.restore_position(pos);
                return self.parse_with_object(parser, Expression::MemberExpression(member_expr), true);
            }
        }

        return Ok(member_expr);
    }

    // Helper method to check if an arrow function is ahead
    fn is_arrow_function_ahead(&self, parser: &mut Parser) -> bool {
        // Save position
        let pos = parser.save_position();
        
        // Skip the async keyword if present
        let is_async = parser.check(&Token::Async);
        if is_async {
            parser.advance();
            
            // For async arrow functions, we need at least one token after 'async'
            if parser.is_at_end() {
                parser.restore_position(pos);
                return false;
            }
            
            // If 'async' is followed by a line terminator, it's not an arrow function
            if parser.previous_line_terminator() {
                parser.restore_position(pos);
                return false;
            }
        }
        
        // Check for single parameter without parentheses
        if matches!(parser.peek(), Token::Identifier(_)) {
            let id_pos = parser.save_position();
            parser.advance(); // Skip identifier
            
            if parser.check(&Token::Arrow) {
                parser.restore_position(pos);
                return true;
            }
            
            parser.restore_position(id_pos);
        }
        
        // Check for parameters in parentheses
        if !parser.consume(&Token::LeftParen) {
            parser.restore_position(pos);
            return false;
        }
        
        // Empty parameter list
        if parser.consume(&Token::RightParen) {
            let is_arrow = parser.check(&Token::Arrow);
            parser.restore_position(pos);
            return is_arrow;
        }
        
        // Skip parameters and watch for trailing comma
        let mut depth = 1;
        let mut had_comma = false;
        
        while depth > 0 && !parser.is_at_end() {
            match parser.peek() {
                Token::LeftParen => {
                    depth += 1;
                    parser.advance();
                },
                Token::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if we just saw a comma before this right paren
                        if had_comma {
                            // This is a trailing comma in parameter list
                            parser.advance(); // Consume the right paren
                            let is_arrow = parser.check(&Token::Arrow);
                            parser.restore_position(pos);
                            return is_arrow;
                        }
                    }
                    parser.advance();
                },
                Token::Comma => {
                    had_comma = true;
                    parser.advance();
                },
                _ => {
                    had_comma = false;
                    parser.advance();
                }
            }
        }
        
        // Check if the next token is an arrow
        let is_arrow = parser.check(&Token::Arrow);
        
        // Restore position
        parser.restore_position(pos);
        
        is_arrow
    }

    /// Get the precedence of the current token
    fn get_precedence(&self, parser: &mut Parser) -> Precedence {
        match parser.peek() {
            Token::Comma => Precedence::Comma,

            Token::Equal |
            Token::PlusEqual |
            Token::MinusEqual |
            Token::StarEqual |
            Token::SlashEqual |
            Token::PercentEqual |
            Token::StarStarEqual |
            Token::LessLessEqual | 
            Token::GreaterGreaterEqual |
            Token::GreaterGreaterGreaterEqual |
            Token::AmpersandEqual |
            Token::PipeEqual |
            Token::CaretEqual |
            Token::AmpersandAmpersandEqual |
            Token::PipePipeEqual |
            Token::QuestionQuestionEqual => Precedence::Assignment,
            
            Token::Question => Precedence::Conditional,
            
            Token::QuestionQuestion => Precedence::NullishCoalescing,
            Token::PipePipe => Precedence::LogicalOr,
            Token::AmpersandAmpersand => Precedence::LogicalAnd,
            
            Token::Pipe => Precedence::BitwiseOr,
            Token::Caret => Precedence::BitwiseXor,
            Token::Ampersand => Precedence::BitwiseAnd,
            
            Token::EqualEqual |
            Token::BangEqual |
            Token::EqualEqualEqual |
            Token::BangEqualEqual => Precedence::Equality,
            
            Token::Less |
            Token::LessEqual |
            Token::Greater |
            Token::GreaterEqual |
            Token::In |
            Token::InstanceOf => Precedence::Relational,
            
            Token::LessLess |
            Token::GreaterGreater |
            Token::GreaterGreaterGreater => Precedence::Shift,
            
            Token::Plus |
            Token::Minus => Precedence::Additive,
            
            Token::Star |
            Token::Slash |
            Token::Percent => Precedence::Multiplicative,
            
            Token::StarStar => Precedence::Exponentiation,
            
            Token::PlusPlus |
            Token::MinusMinus => Precedence::Postfix,
            
            Token::Dot |
            Token::QuestionDot |
            Token::LeftBracket |  
            Token::LeftParen => Precedence::Call,
            
            _ => Precedence::Lowest,
        }
    }
    
    /// Parse a prefix update expression (++x, --x)
    fn parse_prefix_update_expression(&self, parser: &mut Parser) -> ParseResult<Expression> {
        // Parse the operator
        let operator = match parser.peek() {
            Token::PlusPlus => {
                parser.advance();
                UpdateOperator::Increment
            },
            Token::MinusMinus => {
                parser.advance();
                UpdateOperator::Decrement
            },
            _ => return Err(parser.error_at_current("Expected '++' or '--'")),
        };
        
        // Parse the argument
        let argument = self.parse_with_precedence(parser, Precedence::Prefix)?;
        
        // Check that the argument is a valid left-hand side expression
        if !self.is_valid_lhs_expression(&argument) {
            return Err(parser.error_at_current("Invalid left-hand side in prefix operation"));
        }
        
        Ok(Expression::UpdateExpression(UpdateExpression {
            operator,
            argument: Box::new(argument),
            prefix: true,
        }))
    }
    
    /// Parse a postfix update expression (x++, x--)
    fn parse_postfix_update_expression(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        // Check that the left expression is a valid left-hand side expression
        if !self.is_valid_lhs_expression(&left) {
            return Err(parser.error_at_current("Invalid left-hand side in postfix operation"));
        }
        
        // Parse the operator
        let operator = match parser.peek() {
            Token::PlusPlus => {
                parser.advance();
                UpdateOperator::Increment
            },
            Token::MinusMinus => {
                parser.advance();
                UpdateOperator::Decrement
            },
            _ => return Err(parser.error_at_current("Expected '++' or '--'")),
        };
        
        Ok(Expression::UpdateExpression(UpdateExpression {
            operator,
            argument: Box::new(left),
            prefix: false,
        }))
    }
    
    /// Parse a unary expression
    fn parse_unary_expression(&self, parser: &mut Parser) -> ParseResult<Expression> {
        // Parse the operator
        let operator = match parser.peek() {
            Token::Plus => {
                parser.advance();
                UnaryOperator::Plus
            },
            Token::Minus => {
                parser.advance();
                UnaryOperator::Minus
            },
            Token::Bang => {
                parser.advance();
                UnaryOperator::Not
            },
            Token::Tilde => {
                parser.advance();
                UnaryOperator::BitwiseNot
            },
            Token::Typeof => {
                parser.advance();
                UnaryOperator::Typeof
            },
            Token::Void => {
                parser.advance();
                UnaryOperator::Void
            },
            Token::Delete => {
                parser.advance();
                UnaryOperator::Delete
            },
            _ => return Err(parser.error_at_current("Expected a unary operator")),
        };
        
        // Parse the argument
        let argument = self.parse_with_precedence(parser, Precedence::Prefix)?;
        
        Ok(Expression::UnaryExpression(UnaryExpression {
            operator,
            argument: Box::new(argument),
            prefix: true,
        }))
    }
    
    /// Parse a binary expression
    fn parse_binary_expression(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        let (operator, precedence) = self.get_binary_operator(parser)?;
        
        parser.advance();

        let right = self.parse_with_precedence(parser, precedence.next())?;
        
        Ok(Expression::BinaryExpression(BinaryExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }
    
    /// Parse a logical expression
    fn parse_logical_expression(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        let (operator, precedence) = self.get_logical_operator(parser)?;
        
        parser.advance();
        
        let right = self.parse_with_precedence(parser, precedence)?;
        
        Ok(Expression::LogicalExpression(LogicalExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }
    
    /// Parse a conditional expression (ternary)
    fn parse_conditional_expression(&self, parser: &mut Parser, test: Expression) -> ParseResult<Expression> {
        // Consume the question mark
        parser.assert_consume(&Token::Question, "Expected '?' in conditional expression")?;
        
        // Parse the consequent
        let consequent = self.parse_with_precedence(parser, Precedence::Lowest)?;
        
        // Consume the colon
        parser.assert_consume(&Token::Colon, "Expected ':' in conditional expression")?;
        
        // Parse the alternate
        let alternate = self.parse_with_precedence(parser, Precedence::Assignment)?;
        
        Ok(Expression::ConditionalExpression(ConditionalExpression {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate: Box::new(alternate),
        }))
    }
    
    /// Parse an assignment expression
    fn parse_assignment_expression(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        // Get the operator
        let operator = self.get_assignment_operator(parser)?;
        
        // Consume the operator token
        parser.advance();
        
        // Parse the right side
        let right = self.parse_with_precedence(parser, Precedence::Assignment)?;
        
        // Convert the left expression to a valid assignment target
        let left = match self.to_assignment_target(left) {
            Ok(target) => target,
            Err(_) => return Err(parser.error_at_current("Invalid left-hand side in assignment")),
        };
        
        Ok(Expression::AssignmentExpression(AssignmentExpression {
            operator,
            left,
            right: Box::new(right),
        }))
    }
    
    /// Parse a sequence expression (comma-separated expressions)
    fn parse_sequence_expression(&self, parser: &mut Parser, first: Expression) -> ParseResult<Expression> {
        let mut expressions = vec![first];
        
        // Consume the comma
        parser.assert_consume(&Token::Comma, "Expected ',' in sequence expression")?;
        
        // Parse the next expression
        let next = self.parse_with_precedence(parser, Precedence::Lowest)?;
        expressions.push(next);
        
        // Parse any additional expressions
        while parser.consume(&Token::Comma) {
            let expr = self.parse_with_precedence(parser, Precedence::Lowest)?;
            expressions.push(expr);
        }
        
        Ok(Expression::SequenceExpression(SequenceExpression { expressions }))
    }
    
    /// Helper method to get a binary operator and its precedence
    fn get_binary_operator(&self, parser: &mut Parser) -> ParseResult<(BinaryOperator, Precedence)> {
        match parser.peek() {
            Token::Plus => Ok((BinaryOperator::Addition, Precedence::Additive)),
            Token::Minus => Ok((BinaryOperator::Subtraction, Precedence::Additive)),
            Token::Star => Ok((BinaryOperator::Multiplication, Precedence::Multiplicative)),
            Token::Slash => Ok((BinaryOperator::Division, Precedence::Multiplicative)),
            Token::Percent => Ok((BinaryOperator::Remainder, Precedence::Multiplicative)),
            Token::StarStar => Ok((BinaryOperator::Exponentiation, Precedence::Exponentiation)),
            Token::LessLess => Ok((BinaryOperator::LeftShift, Precedence::Shift)),
            Token::GreaterGreater => Ok((BinaryOperator::RightShift, Precedence::Shift)),
            Token::GreaterGreaterGreater => Ok((BinaryOperator::UnsignedRightShift, Precedence::Shift)),
            Token::Ampersand => Ok((BinaryOperator::BitwiseAnd, Precedence::BitwiseAnd)),
            Token::Pipe => Ok((BinaryOperator::BitwiseOr, Precedence::BitwiseOr)),
            Token::Caret => Ok((BinaryOperator::BitwiseXor, Precedence::BitwiseXor)),
            Token::EqualEqual => Ok((BinaryOperator::Equal, Precedence::Equality)),
            Token::BangEqual => Ok((BinaryOperator::NotEqual, Precedence::Equality)),
            Token::EqualEqualEqual => Ok((BinaryOperator::StrictEqual, Precedence::Equality)),
            Token::BangEqualEqual => Ok((BinaryOperator::StrictNotEqual, Precedence::Equality)),
            Token::Less => Ok((BinaryOperator::LessThan, Precedence::Relational)),
            Token::LessEqual => Ok((BinaryOperator::LessThanOrEqual, Precedence::Relational)),
            Token::Greater => Ok((BinaryOperator::GreaterThan, Precedence::Relational)),
            Token::GreaterEqual => Ok((BinaryOperator::GreaterThanOrEqual, Precedence::Relational)),
            Token::In => Ok((BinaryOperator::In, Precedence::Relational)),
            Token::InstanceOf => Ok((BinaryOperator::InstanceOf, Precedence::Relational)),
            _ => Err(parser.error_at_current("Expected a binary operator")),
        }
    }

    /// Helper method to get a logical operator and its precedence
    fn get_logical_operator(&self, parser: &mut Parser) -> ParseResult<(LogicalOperator, Precedence)> {
        match parser.peek() {
            Token::AmpersandAmpersand => Ok((LogicalOperator::And, Precedence::LogicalAnd)),
            Token::PipePipe => Ok((LogicalOperator::Or, Precedence::LogicalOr)),
            Token::QuestionQuestion => Ok((LogicalOperator::NullishCoalescing, Precedence::NullishCoalescing)),
            _ => Err(parser.error_at_current("Expected a logical operator")),
        }
    }
    
    /// Helper method to get an assignment operator
    fn get_assignment_operator(&self, parser: &mut Parser) -> ParseResult<AssignmentOperator> {
        match parser.peek() {
            Token::Equal => Ok(AssignmentOperator::Assign),
            Token::PlusEqual => Ok(AssignmentOperator::PlusAssign),
            Token::MinusEqual => Ok(AssignmentOperator::MinusAssign),
            Token::StarEqual => Ok(AssignmentOperator::MultiplyAssign),
            Token::SlashEqual => Ok(AssignmentOperator::DivideAssign),
            Token::PercentEqual => Ok(AssignmentOperator::RemainderAssign),
            Token::StarStarEqual => Ok(AssignmentOperator::ExponentiationAssign),
            Token::LessLessEqual => Ok(AssignmentOperator::LeftShiftAssign),
            Token::GreaterGreaterEqual => Ok(AssignmentOperator::RightShiftAssign),
            Token::GreaterGreaterGreaterEqual => Ok(AssignmentOperator::UnsignedRightShiftAssign),
            Token::AmpersandEqual => Ok(AssignmentOperator::BitwiseAndAssign),
            Token::PipeEqual => Ok(AssignmentOperator::BitwiseOrAssign),
            Token::CaretEqual => Ok(AssignmentOperator::BitwiseXorAssign),
            Token::AmpersandAmpersandEqual => Ok(AssignmentOperator::LogicalAndAssign),
            Token::PipePipeEqual => Ok(AssignmentOperator::LogicalOrAssign),
            Token::QuestionQuestionEqual => Ok(AssignmentOperator::NullishCoalescingAssign),
            _ => Err(parser.error_at_current("Expected an assignment operator")),
        }
    }
    
    /// Helper method to convert an expression to an assignment target
    fn to_assignment_target(&self, expr: Expression) -> Result<AssignmentLeft, ()> {
        match expr {
            Expression::Identifier(_) => {
                // Convert to pattern
                Ok(AssignmentLeft::Pattern(Pattern::Identifier(match expr {
                    Expression::Identifier(ident) => ident,
                    _ => unreachable!(),
                })))
            },
            Expression::MemberExpression(member) => {
                // Member expressions are valid assignment targets
                Ok(AssignmentLeft::Expression(Box::new(Expression::MemberExpression(member))))
            },
            _ => Err(()),
        }
    }
    
    /// Helper method to check if an expression is a valid left-hand side expression
    fn is_valid_lhs_expression(&self, expr: &Expression) -> bool {
        match expr {
            Expression::Identifier(_) | 
            Expression::MemberExpression(_) => true,
            _ => false,
        }
    }
}

impl ParserCombinator<Expression> for ExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Expression> {
        self.parse_with_precedence(parser, Precedence::Lowest)
    }
}

/// Precedence levels for expression parsing
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Comma,           // ,
    Assignment,      // = += -= etc.
    Conditional,     // ?:
    NullishCoalescing, // ??
    LogicalOr,       // ||
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
    Prefix,          // ! ~ + - typeof void delete ++x --x
    Postfix,         // x++ x--
    Call,            // . [] ()
}

impl Precedence {
    /// Get the next higher precedence level
    pub fn next(&self) -> Self {
        match self {
            Precedence::Lowest => Precedence::Comma,
            Precedence::Comma => Precedence::Assignment,
            Precedence::Assignment => Precedence::Conditional,
            Precedence::Conditional => Precedence::NullishCoalescing,
            Precedence::NullishCoalescing => Precedence::LogicalOr,
            Precedence::LogicalOr => Precedence::LogicalAnd,
            Precedence::LogicalAnd => Precedence::BitwiseOr,
            Precedence::BitwiseOr => Precedence::BitwiseXor,
            Precedence::BitwiseXor => Precedence::BitwiseAnd,
            Precedence::BitwiseAnd => Precedence::Equality,
            Precedence::Equality => Precedence::Relational,
            Precedence::Relational => Precedence::Shift,
            Precedence::Shift => Precedence::Additive,
            Precedence::Additive => Precedence::Multiplicative,
            Precedence::Multiplicative => Precedence::Exponentiation,
            Precedence::Exponentiation => Precedence::Prefix,
            Precedence::Prefix => Precedence::Postfix,
            Precedence::Postfix => Precedence::Call,
            Precedence::Call => Precedence::Call, // Can't go higher
        }
    }
}

// Main expression unparser
impl UnparserCombinator<Expression> for ExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &Expression) {
        match node {
            Expression::Identifier(ident) => {
                unparser.write_str(&ident.name);
            },
            Expression::Literal(lit) => {
                self.unparse_literal(unparser, lit);
            },
            Expression::ArrayExpression(array) => {
                ArrayExpressionNode::new().unparse(unparser, array);
            },
            Expression::ObjectExpression(obj) => {
                ObjectExpressionNode::new().unparse(unparser, obj);
            },
            Expression::FunctionExpression(func) => {
                FunctionExpressionNode::new().unparse(unparser, func);
            },
            Expression::ArrowFunctionExpression(arrow) => {
                ArrowFunctionExpressionNode::new().unparse(unparser, arrow);
            },
            Expression::ClassExpression(class) => {
                ClassExpressionNode::new().unparse(unparser, class);
            },
            Expression::TaggedTemplateExpression(tagged) => {
                self.unparse_tagged_template(unparser, tagged);
            },
            Expression::MemberExpression(member) => {
                self.unparse_member_expression(unparser, member);
            },
            Expression::SuperExpression(super_expr) => {
                SuperExpressionNode::new().unparse(unparser, super_expr);
            },
            Expression::MetaProperty(meta) => {
                self.unparse_meta_property(unparser, meta);
            },
            Expression::NewExpression(new_expr) => {
                self.unparse_new_expression(unparser, new_expr);
            },
            Expression::CallExpression(call) => {
                self.unparse_call_expression(unparser, call);
            },
            Expression::UpdateExpression(update) => {
                self.unparse_update_expression(unparser, update);
            },
            Expression::AwaitExpression(await_expr) => {
                AwaitExpressionNode::new().unparse(unparser, await_expr);
            },
            Expression::UnaryExpression(unary) => {
                self.unparse_unary_expression(unparser, unary);
            },
            Expression::BinaryExpression(binary) => {
                self.unparse_binary_expression(unparser, binary);
            },
            Expression::LogicalExpression(logical) => {
                self.unparse_logical_expression(unparser, logical);
            },
            Expression::ConditionalExpression(cond) => {
                self.unparse_conditional_expression(unparser, cond);
            },
            Expression::YieldExpression(yield_expr) => {
                YieldExpressionNode::new().unparse(unparser, yield_expr);
            },
            Expression::AssignmentExpression(assign) => {
                self.unparse_assignment_expression(unparser, assign);
            },
            Expression::SequenceExpression(seq) => {
                self.unparse_sequence_expression(unparser, seq);
            },
            Expression::ThisExpression(this) => {
                ThisExpressionNode::new().unparse(unparser, this);
            },
            // TODO implement
//            Expression::TemplateLiteral(template) => {
//                self.unparse_template_literal(unparser, template);
//            },
            //_ => {
                // Fallback for any expression types not explicitly handled
              //  unparser.write_str("/* unsupported expression */");
            //}
        }
    }
}

// Helper methods for ExpressionNode
impl ExpressionNode {
    fn unparse_literal(&self, unparser: &mut Unparser, lit: &Literal) {
        match lit {
            Literal::StringLiteral(s) => {
                unparser.write_char('"');
                unparser.write_str(&s.value);
                unparser.write_char('"');
            },
            Literal::NumericLiteral(n) => {
                unparser.write_str(&n.value.to_string());
            },
            Literal::BooleanLiteral(b) => {
                unparser.write_str(if b.value { "true" } else { "false" });
            },
            Literal::NullLiteral(_) => {
                unparser.write_str("null");
            },
            Literal::UndefinedLiteral(_) => {
                unparser.undefined();
            },
            Literal::RegExpLiteral(r) => {
                unparser.write_char('/');
                unparser.write_str(&r.pattern);
                unparser.write_char('/');
                unparser.write_str(&r.flags);
            },
            Literal::BigIntLiteral(b) => {
                unparser.write_str(&b.value);
                unparser.write_char('n');
            }
        }
    }

    fn unparse_tagged_template(&self, unparser: &mut Unparser, tagged: &TaggedTemplateExpression) {
        // Unparse the tag
        self.unparse(unparser, &tagged.tag);
        
        // Unparse the template literal
        self.unparse_template_literal(unparser, &tagged.quasi);
    }

    fn unparse_template_literal(&self, unparser: &mut Unparser, template: &TemplateLiteral) {
        unparser.write_char('`');
        
        for (i, elem) in template.quasis.iter().enumerate() {
            // Write the template string part
            unparser.write_str(&elem.value.raw);
            
            // If there's an expression after this quasi, write it
            if i < template.expressions.len() {
                unparser.write_str("${");
                self.unparse(unparser, &template.expressions[i]);
                unparser.write_char('}');
            }
        }
        
        unparser.write_char('`');
    }

    fn unparse_member_expression(&self, unparser: &mut Unparser, member: &MemberExpression) {
        // Unparse the object
        self.unparse(unparser, &member.object);
        
        // Handle optional chaining
        if member.optional {
            unparser.write_str("?.");
        }
        
        // Unparse the property
        match &member.property {
            MemberProperty::Identifier(id) => {
                if !member.optional {
                    unparser.write_char('.');
                }
                unparser.write_str(&id.name);
            },
            MemberProperty::PrivateIdentifier(id) => {
                // Handle private identifiers (class private fields/methods)
                unparser.write_char('#');
                unparser.write_str(&id.name);
            },
            MemberProperty::Expression(expr) => {
                unparser.write_char('[');
                self.unparse(unparser, expr);
                unparser.write_char(']');
            }
        }
    }

    fn unparse_meta_property(&self, unparser: &mut Unparser, meta: &MetaProperty) {
        unparser.write_str(&meta.meta.name);
        unparser.write_char('.');
        unparser.write_str(&meta.property.name);
    }

    fn unparse_new_expression(&self, unparser: &mut Unparser, new_expr: &NewExpression) {
        unparser.write_str("new ");
        
        // Unparse the callee
        self.unparse(unparser, &new_expr.callee);
        
        // Unparse the arguments
        unparser.write_char('(');
        
        for (i, arg) in new_expr.arguments.iter().enumerate() {
            if i > 0 {
                unparser.write_char(',');
                unparser.space();
            }
            self.unparse(unparser, arg);
        }
        
        unparser.write_char(')');
    }

    fn unparse_call_expression(&self, unparser: &mut Unparser, call: &CallExpression) {
        // Unparse the callee
        self.unparse(unparser, &call.callee);
        
        // Handle optional chaining
        if call.optional {
            unparser.write_str("?.");
        }
        
        // Unparse the arguments
        unparser.write_char('(');
        
        for (i, arg) in call.arguments.iter().enumerate() {
            if i > 0 {
                unparser.write_char(',');
                unparser.space();
            }
            self.unparse(unparser, arg);
        }
        
        unparser.write_char(')');
    }

    fn unparse_update_expression(&self, unparser: &mut Unparser, update: &UpdateExpression) {
        let operator_str = match update.operator {
            UpdateOperator::Increment => "++",
            UpdateOperator::Decrement => "--",
        };
        
        if update.prefix {
            unparser.write_str(operator_str);
            self.unparse(unparser, &update.argument);
        } else {
            self.unparse(unparser, &update.argument);
            unparser.write_str(operator_str);
        }
    }

    fn unparse_unary_expression(&self, unparser: &mut Unparser, unary: &UnaryExpression) {
        let operator_str = match unary.operator {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "!",
            UnaryOperator::BitwiseNot => "~",
            UnaryOperator::Typeof => "typeof ",
            UnaryOperator::Void => "void ",
            UnaryOperator::Delete => "delete ",
        };
        
        unparser.write_str(operator_str);
        
        // Determine if we need parentheses
        let needs_parens = matches!(&*unary.argument, 
            Expression::UnaryExpression(_) | 
            Expression::BinaryExpression(_) | 
            Expression::LogicalExpression(_) |
            Expression::ConditionalExpression(_) |
            Expression::AssignmentExpression(_)
        ) && !matches!(unary.operator, UnaryOperator::Typeof | UnaryOperator::Void | UnaryOperator::Delete);
        
        // Unparse the argument
        if needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &unary.argument);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &unary.argument);
        }
    }


    fn unparse_binary_expression(&self, unparser: &mut Unparser, binary: &BinaryExpression) {
        let operator_str = match binary.operator {
            BinaryOperator::Addition => "+",
            BinaryOperator::Subtraction => "-",
            BinaryOperator::Multiplication => "*",
            BinaryOperator::Division => "/",
            BinaryOperator::Remainder => "%",
            BinaryOperator::Exponentiation => "**",
            BinaryOperator::LeftShift => "<<",
            BinaryOperator::RightShift => ">>",
            BinaryOperator::UnsignedRightShift => ">>>",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::StrictEqual => "===",
            BinaryOperator::StrictNotEqual => "!==",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanOrEqual => ">=",
            BinaryOperator::In => " in ",
            BinaryOperator::InstanceOf => " instanceof ",
        };
        
        // Determine if we need parentheses based on operator precedence
        let left_needs_parens = self.needs_parentheses(&binary.left, &binary.operator, true);
        let right_needs_parens = self.needs_parentheses(&binary.right, &binary.operator, false);
        
        // Unparse left operand
        if left_needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &binary.left);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &binary.left);
        }
        
        // Add space before operator for readability
        if !matches!(binary.operator, BinaryOperator::In | BinaryOperator::InstanceOf) {
            unparser.space();
        }
        
        // Write the operator
        unparser.write_str(operator_str);
        
        // Add space after operator for readability
        if !matches!(binary.operator, BinaryOperator::In | BinaryOperator::InstanceOf) {
            unparser.space();
        }
        
        // Unparse right operand
        if right_needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &binary.right);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &binary.right);
        }
    }

    fn unparse_logical_expression(&self, unparser: &mut Unparser, logical: &LogicalExpression) {
        let operator_str = match logical.operator {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
            LogicalOperator::NullishCoalescing => "??",
        };
        
        // Determine if we need parentheses based on operator precedence
        let left_needs_parens = self.needs_logical_parentheses(&logical.left, &logical.operator, true);
        let right_needs_parens = self.needs_logical_parentheses(&logical.right, &logical.operator, false);
        
        // Unparse left operand
        if left_needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &logical.left);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &logical.left);
        }
        
        // Add space before operator
        unparser.space();
        
        // Write the operator
        unparser.write_str(operator_str);
        
        // Add space after operator
        unparser.space();
        
        // Unparse right operand
        if right_needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &logical.right);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &logical.right);
        }
    }

    fn unparse_conditional_expression(&self, unparser: &mut Unparser, cond: &ConditionalExpression) {
        // Determine if test needs parentheses
        let test_needs_parens = matches!(&*cond.test,
            Expression::AssignmentExpression(_) |
            Expression::ConditionalExpression(_) |
            Expression::SequenceExpression(_)
        );
        
        // Unparse test expression
        if test_needs_parens {
            unparser.write_char('(');
            self.unparse(unparser, &cond.test);
            unparser.write_char(')');
        } else {
            self.unparse(unparser, &cond.test);
        }
        
        // Write the question mark
        unparser.space();
        unparser.write_char('?');
        unparser.space();
        
        // Unparse consequent expression
        self.unparse(unparser, &cond.consequent);
        
        // Write the colon
        unparser.space();
        unparser.write_char(':');
        unparser.space();
        
        // Unparse alternate expression
        self.unparse(unparser, &cond.alternate);
    }
    
    fn unparse_assignment_expression(&self, unparser: &mut Unparser, assign: &AssignmentExpression) {
        // Unparse the left side
        match &assign.left {
            AssignmentLeft::Pattern(pattern) => {
                PatternNode::new().unparse(unparser, pattern);
            },
            AssignmentLeft::Expression(expr) => {
                self.unparse(unparser, expr);
            }
        }
        
        // Write the operator
        unparser.space();
        match assign.operator {
            AssignmentOperator::Assign => unparser.write_char('='),
            AssignmentOperator::PlusAssign => unparser.write_str("+="),
            AssignmentOperator::MinusAssign => unparser.write_str("-="),
            AssignmentOperator::MultiplyAssign => unparser.write_str("*="),
            AssignmentOperator::DivideAssign => unparser.write_str("/="),
            AssignmentOperator::RemainderAssign => unparser.write_str("%="),
            AssignmentOperator::ExponentiationAssign => unparser.write_str("**="),
            AssignmentOperator::LeftShiftAssign => unparser.write_str("<<="),
            AssignmentOperator::RightShiftAssign => unparser.write_str(">>="),
            AssignmentOperator::UnsignedRightShiftAssign => unparser.write_str(">>>="),
            AssignmentOperator::BitwiseAndAssign => unparser.write_str("&="),
            AssignmentOperator::BitwiseOrAssign => unparser.write_str("|="),
            AssignmentOperator::BitwiseXorAssign => unparser.write_str("^="),
            AssignmentOperator::LogicalAndAssign => unparser.write_str("&&="),
            AssignmentOperator::LogicalOrAssign => unparser.write_str("||="),
            AssignmentOperator::NullishCoalescingAssign => unparser.write_str("??="),
        }
        unparser.space();
        
        // Unparse the right side
        self.unparse(unparser, &assign.right);
    }
    
    fn unparse_sequence_expression(&self, unparser: &mut Unparser, seq: &SequenceExpression) {
        for (i, expr) in seq.expressions.iter().enumerate() {
            if i > 0 {
                unparser.write_char(',');
                unparser.space();
            }
            self.unparse(unparser, expr);
        }
    }
    
    // Helper method to determine if parentheses are needed for binary expressions
    fn needs_parentheses(&self, expr: &Expression, parent_op: &BinaryOperator, is_left: bool) -> bool {
        match expr {
            Expression::BinaryExpression(binary) => {
                let child_precedence = self.get_binary_precedence(&binary.operator);
                let parent_precedence = self.get_binary_precedence(parent_op);
                
                // If the child has lower precedence, we need parentheses
                if child_precedence < parent_precedence {
                    return true;
                }
                
                // If they have the same precedence, we need parentheses for right-associative operators
                // or for the right operand of left-associative operators
                if child_precedence == parent_precedence {
                    // Handle right-associative operators (currently only **)
                    if matches!(parent_op, BinaryOperator::Exponentiation) {
                        return is_left;
                    }
                    // For left-associative operators, need parentheses on right side when precedences are equal
                    return !is_left;
                }
                
                false
            },
            Expression::LogicalExpression(_) |
            Expression::ConditionalExpression(_) |
            Expression::AssignmentExpression(_) |
            Expression::SequenceExpression(_) => true,
            _ => false,
        }
    }
    
    // Helper method to determine if parentheses are needed for logical expressions
    fn needs_logical_parentheses(&self, expr: &Expression, parent_op: &LogicalOperator, is_left: bool) -> bool {
        match expr {
            Expression::LogicalExpression(logical) => {
                let child_precedence = self.get_logical_precedence(&logical.operator);
                let parent_precedence = self.get_logical_precedence(parent_op);
                
                // If the child has lower precedence, we need parentheses
                if child_precedence < parent_precedence {
                    return true;
                }
                
                // If they have the same precedence, we need parentheses for the right operand
                // of left-associative operators (all logical operators are left-associative)
                if child_precedence == parent_precedence && !is_left {
                    return true;
                }
                
                false
            },
            Expression::ConditionalExpression(_) |
            Expression::AssignmentExpression(_) |
            Expression::SequenceExpression(_) => true,
            _ => false,
        }
    }
    
    // Helper method to get binary operator precedence
    fn get_binary_precedence(&self, op: &BinaryOperator) -> u8 {
        match op {
            BinaryOperator::Exponentiation => 14,
            BinaryOperator::Multiplication | BinaryOperator::Division | BinaryOperator::Remainder => 13,
            BinaryOperator::Addition | BinaryOperator::Subtraction => 12,
            BinaryOperator::LeftShift | BinaryOperator::RightShift | BinaryOperator::UnsignedRightShift => 11,
            BinaryOperator::LessThan | BinaryOperator::LessThanOrEqual | 
            BinaryOperator::GreaterThan | BinaryOperator::GreaterThanOrEqual |
            BinaryOperator::In | BinaryOperator::InstanceOf => 10,
            BinaryOperator::Equal | BinaryOperator::NotEqual | 
            BinaryOperator::StrictEqual | BinaryOperator::StrictNotEqual => 9,
            BinaryOperator::BitwiseAnd => 8,
            BinaryOperator::BitwiseXor => 7,
            BinaryOperator::BitwiseOr => 6,
            //_ => 0, // Should not happen
        }
    }
    
    // Helper method to get logical operator precedence
    fn get_logical_precedence(&self, op: &LogicalOperator) -> u8 {
        match op {
            LogicalOperator::And => 5,
            LogicalOperator::Or => 4,
            LogicalOperator::NullishCoalescing => 3,
        }
    }
}
