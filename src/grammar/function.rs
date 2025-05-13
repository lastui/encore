use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::pattern::*;
use super::expression::*;
use super::statement::*;

/// Parser for function expressions
pub struct FunctionExpressionParser;

impl FunctionExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<FunctionExpression> for FunctionExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<FunctionExpression> {        
        // Consume the 'function' keyword
        parser.assert_consume(&Token::Function, "Expected 'function'")?;
        
        // Check if this is a generator function
        let generator = parser.consume(&Token::Star);
        
        // Parse the function name if present
        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };

        // Parse the parameter list
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !parser.check(&Token::RightParen) {
            // Parse the first parameter
            params.push(PatternParser::new().parse(parser)?);
            
            // Parse additional parameters
            while parser.consume(&Token::Comma) {
                params.push(PatternParser::new().parse(parser)?);
            }
        }
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after function parameters")?;

        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: false, allow_yield: generator }, |p| {
            BlockStatementParser::new().parse(p)
        })?;
        
        Ok(FunctionExpression {
            id,
            params,
            body,
            generator,
            async_function: false,
        })
    }
}

/// Parser for async function expressions
pub struct AsyncFunctionExpressionParser;

impl AsyncFunctionExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<FunctionExpression> for AsyncFunctionExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<FunctionExpression> {
        // Consume the 'async' keyword
        parser.assert_consume(&Token::Async, "Expected 'async'")?;
        
        // Consume the 'function' keyword
        parser.assert_consume(&Token::Function, "Expected 'function' after 'async'")?;
        
        // Check if this is a generator function
        let generator = parser.consume(&Token::Star);
        
        // Parse the function name if present
        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };

        // Parse the parameter list
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !parser.check(&Token::RightParen) {
            // Parse the first parameter
            params.push(PatternParser::new().parse(parser)?);
            
            // Parse additional parameters
            while parser.consume(&Token::Comma) {
                params.push(PatternParser::new().parse(parser)?);
            }
        }
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after function parameters")?;

        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: true, allow_yield: generator }, |p| {
            BlockStatementParser::new().parse(p)
        })?;

        Ok(FunctionExpression {
            id,
            params,
            body,
            generator,
            async_function: true,
        })
    }
}

/// Parser for arrow function expressions
pub struct ArrowFunctionExpressionParser;

impl ArrowFunctionExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrowFunctionExpression> for ArrowFunctionExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrowFunctionExpression> {
        // Check for async arrow function
        let async_function = parser.consume(&Token::Async);
        
        // Parse the parameters
        let mut params = Vec::new();
        
        if parser.consume(&Token::LeftParen) {
            // Multiple parameters in parentheses
            if !parser.check(&Token::RightParen) {
                // Parse the first parameter
                params.push(PatternParser::new().parse(parser)?);
                
                // Parse additional parameters
                while parser.consume(&Token::Comma) {
                    // Check for trailing comma (right paren immediately after comma)
                    if parser.check(&Token::RightParen) {
                        break; // This is a trailing comma, so stop parsing parameters
                    }
                    
                    // Parse the next parameter
                    params.push(PatternParser::new().parse(parser)?);
                }
            }
            
            parser.assert_consume(&Token::RightParen, "Expected ')' after arrow function parameters")?;
        } else {
            // Single parameter without parentheses
            params.push(PatternParser::new().parse(parser)?);
        }
        
        // Consume the arrow
        parser.assert_consume(&Token::Arrow, "Expected '=>' after arrow function parameters")?;
        
        let block = parser.check(&Token::LeftBrace);

        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: async_function, allow_yield: false }, |p| {
            if p.check(&Token::LeftBrace) {
                let block = BlockStatementParser::new().parse(p)?;
                Ok(ArrowFunctionBody::BlockStatement(block))
            } else {
                let expr = ExpressionParser::new().parse(p)?;
                Ok(ArrowFunctionBody::Expression(Box::new(expr)))
            }
        })?;

        Ok(ArrowFunctionExpression {
            params,
            body,
            expression: !block,
            async_function,
        })
    }
}


/// Parser for await expressions
pub struct AwaitExpressionParser;

impl AwaitExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<AwaitExpression> for AwaitExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<AwaitExpression> {        
        // Consume the 'await' keyword
        parser.assert_consume(&Token::Await, "Expected 'await'")?;
        
        // Parse the argument
        let argument = Box::new(ExpressionParser::new().parse(parser)?);

        Ok(AwaitExpression {
            argument,
        })
    }
}

/// Parser for yield expressions
pub struct YieldExpressionParser;

impl YieldExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<YieldExpression> for YieldExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<YieldExpression> {
        // Consume the 'yield' keyword
        parser.assert_consume(&Token::Yield, "Expected 'yield'")?;
        
        // Check for delegate (yield*)
        let delegate = parser.consume(&Token::Star);
        
        // Parse the argument if present
        let argument = if parser.check(&Token::Semicolon) || parser.is_at_end() || 
                         parser.check(&Token::RightBrace) || parser.check(&Token::Comma) ||
                         parser.check(&Token::RightParen) || parser.check(&Token::RightBracket) ||
                         parser.check(&Token::Colon) || parser.previous_line_terminator() {
            None
        } else {
            Some(Box::new(ExpressionParser::new().parse(parser)?))
        };
        
        Ok(YieldExpression {
            argument,
            delegate,
        })
    }
}
