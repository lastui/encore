use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::pattern::*;
use super::expression::*;
use super::statement::*;

pub struct FunctionExpressionNode;

impl FunctionExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<FunctionExpression> for FunctionExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<FunctionExpression> {
        // Check if this is an async function
        let async_function = parser.consume(&Token::Async);
        
        // Consume the 'function' keyword
        parser.assert_consume(&Token::Function, "Expected 'function'")?;
        
        // Check if this is a generator function
        let generator = parser.consume(&Token::Star);
        
        // Parse the function name if present
        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };

        // Parse the parameter list
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !parser.check(&Token::RightParen) {
            // Parse the first parameter
            params.push(PatternNode::new().parse(parser)?);
            
            // Parse additional parameters
            while parser.consume(&Token::Comma) {
                params.push(PatternNode::new().parse(parser)?);
            }
        }
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after function parameters")?;

        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: async_function, allow_yield: generator }, |p| {
            BlockStatementNode::new().parse(p)
        })?;
        
        Ok(FunctionExpression {
            id,
            params,
            body,
            generator,
            async_function,
        })
    }
}


impl UnparserCombinator<FunctionExpression> for FunctionExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &FunctionExpression) {
        // Write async if it's an async function
        if node.async_function {
            unparser.write_str("async");
            unparser.write_char(' ');
        }
        
        // Write the function keyword
        unparser.write_str("function");
        
        // Write * if it's a generator function
        if node.generator {
            unparser.write_char('*');
        }
        
        // Write the function name if present
        if let Some(id) = &node.id {
            //unparser.space();
            unparser.write_char(' ');
            unparser.write_str(&id.name);
        }
        
        // Write the parameter list
        unparser.write_char('(');
        
        // Write parameters
        if !node.params.is_empty() {
            PatternNode::new().unparse(unparser, &node.params[0]);
            
            for param in &node.params[1..] {
                unparser.write_char(',');
                unparser.space();
                PatternNode::new().unparse(unparser, param);
            }
        }
        
        unparser.write_char(')');
        unparser.space();
        
        // Write the function body
        BlockStatementNode::new().unparse(unparser, &node.body);
    }
}

/// Parser for arrow function expressions
pub struct ArrowFunctionExpressionNode;

impl ArrowFunctionExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ArrowFunctionExpression> for ArrowFunctionExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ArrowFunctionExpression> {
        // Check for async arrow function
        let async_function = parser.consume(&Token::Async);
        
        // Parse the parameters
        let mut params = Vec::new();
        
        if parser.consume(&Token::LeftParen) {
            // Multiple parameters in parentheses
            if !parser.check(&Token::RightParen) {
                // Parse the first parameter
                params.push(PatternNode::new().parse(parser)?);
                
                // Parse additional parameters
                while parser.consume(&Token::Comma) {
                    // Check for trailing comma (right paren immediately after comma)
                    if parser.check(&Token::RightParen) {
                        break; // This is a trailing comma, so stop parsing parameters
                    }
                    
                    // Parse the next parameter
                    params.push(PatternNode::new().parse(parser)?);
                }
            }
            
            parser.assert_consume(&Token::RightParen, "Expected ')' after arrow function parameters")?;
        } else {
            // Single parameter without parentheses
            params.push(PatternNode::new().parse(parser)?);
        }
        
        // Consume the arrow
        parser.assert_consume(&Token::Arrow, "Expected '=>' after arrow function parameters")?;
        
        let block = parser.check(&Token::LeftBrace);

        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: async_function, allow_yield: false }, |p| {
            if p.check(&Token::LeftBrace) {
                let block = BlockStatementNode::new().parse(p)?;
                Ok(ArrowFunctionBody::BlockStatement(block))
            } else {
                let expr = ExpressionNode::new().parse(p)?;
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

impl UnparserCombinator<ArrowFunctionExpression> for ArrowFunctionExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ArrowFunctionExpression) {
        // Write async if it's an async arrow function
        if node.async_function {
            unparser.write_str("async");
            unparser.space();
        }
        
        // Write the parameter list
        if node.params.len() == 1 && node.expression {
            // Single parameter without parentheses for expression body arrow functions
            PatternNode::new().unparse(unparser, &node.params[0]);
        } else {
            // Multiple parameters or block body requires parentheses
            unparser.write_char('(');
            
            if !node.params.is_empty() {
                PatternNode::new().unparse(unparser, &node.params[0]);
                
                for param in &node.params[1..] {
                    unparser.write_char(',');
                    unparser.space();
                    PatternNode::new().unparse(unparser, param);
                }
            }
            
            unparser.write_char(')');
        }
        
        // Write the arrow
        unparser.space();
        unparser.write_str("=>");
        unparser.space();
        
        // Write the function body
        match &node.body {
            ArrowFunctionBody::BlockStatement(block) => {
                BlockStatementNode::new().unparse(unparser, block);
            },
            ArrowFunctionBody::Expression(expr) => {
                ExpressionNode::new().unparse(unparser, expr);
            }
        }
    }
}
