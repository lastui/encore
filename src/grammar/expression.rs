use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::function::*;
use super::call::*;
use super::class::*;
use super::array::*;
use super::member::*;
use super::pattern::*;
use super::literal::*;
use super::object::*;
use super::this::*;
use super::new::*;


/// Parser for JavaScript expressions
pub struct ExpressionParser;

impl ExpressionParser {
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
                let ident = IdentifierParser::new().parse(parser)?;
                
                if parser.check(&Token::Arrow) {
                    // This is an arrow function with a single parameter
                    parser.restore_position(pos);
                    return ArrowFunctionExpressionParser::new().parse(parser).map(Expression::ArrowFunctionExpression);
                }
                
                Ok(Expression::Identifier(ident))
            },
            Token::StringLiteral(_) | 
            Token::NumberLiteral(_) | 
            Token::BigIntLiteral(_) |
            Token::RegExpLiteral(_, _) |
            Token::True |
            Token::False |
            Token::Null => {
                LiteralParser::new().parse(parser).map(Expression::Literal)
            },
            Token::This => {
                ThisExpressionParser::new().parse(parser).map(Expression::ThisExpression)
            },
            Token::LeftBracket => {
                ArrayExpressionParser::new().parse(parser).map(Expression::ArrayExpression)
            },
            Token::LeftBrace => {
                ObjectExpressionParser::new().parse(parser).map(Expression::ObjectExpression)
            },
            Token::Function => {
                FunctionExpressionParser::new().parse(parser).map(Expression::FunctionExpression)
            },
            Token::Class => {
                ClassExpressionParser::new().parse(parser).map(Expression::ClassExpression)
            },
            Token::New => {
                NewExpressionParser::new().parse(parser).map(Expression::NewExpression)
            },
            Token::Super => {
                SuperExpressionParser::new().parse(parser).map(Expression::SuperExpression)
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
                    AwaitExpressionParser::new().parse(parser).map(Expression::AwaitExpression)
                } else {
                    Err(parser.error_at_current("'await' expression is only allowed within async functions"))
                }
            },
            Token::Yield => {
                if parser.allows_yield() {
                    YieldExpressionParser::new().parse(parser).map(Expression::YieldExpression)
                } else {
                    Err(parser.error_at_current("'yield' expression is only allowed within generator functions"))
                }
            },
            Token::Async => {
                // Look ahead to see if this is an async function or arrow function
                if parser.peek_next(1) == &Token::Function || (parser.peek_next(1) == &Token::LeftParen && self.is_arrow_function_ahead(parser)) {
                    AsyncFunctionExpressionParser::new().parse(parser).map(Expression::FunctionExpression)
                } else {
                    // Otherwise, it's just an identifier named "async"
                    IdentifierParser::new().parse(parser).map(Expression::Identifier)
                }
            },
            _ => Err(parser.error_at_current("Expected an expression")),
        }
    }
    
    /// Parse an infix expression
    fn parse_infix(&self, parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
        match parser.peek() {
            Token::LeftParen => {
                CallExpressionParser::new().parse_with_callee(parser, left).map(Expression::CallExpression)
            },
            Token::Dot |
            Token::LeftBracket => {
                MemberExpressionParser::new().parse_with_object(parser, left).map(Expression::MemberExpression)
            },
            Token::QuestionDot => {
                // Optional chaining
                parser.advance(); // Consume the '?.'
                
                // Check if this is an optional function call
                if parser.check(&Token::LeftParen) {
                    // Optional function call: obj?.(args)
                    let call_expr = CallExpressionParser::new().parse_with_callee(parser, left)?;
                    Ok(Expression::CallExpression(call_expr))
                } else {
                    // Optional property access: obj?.prop
                    let member_expr = MemberExpressionParser::new().parse_with_object(parser, left)?;
                    Ok(Expression::MemberExpression(member_expr))
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
            return ArrowFunctionExpressionParser::new().parse(parser).map(Expression::ArrowFunctionExpression);
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

    // Helper method to check if an arrow function is ahead
    fn is_arrow_function_ahead(&self, parser: &mut Parser) -> bool {
        // Save position
        let pos = parser.save_position();
        
        // Skip the async keyword if present
        if parser.check(&Token::Async) {
            parser.advance();
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

impl ParserCombinator<Expression> for ExpressionParser {
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
