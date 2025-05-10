use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;
use super::pattern::*;
use super::statement::*;
use super::function::*;
use super::literal::*;

pub struct ClassDeclarationParser;

impl ClassDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassDeclaration> for ClassDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassDeclaration> {
        parser.assert_consume(&Token::Class, "Expected 'class'")?;

        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };

        let super_class = if parser.consume(&Token::Extends) {
            Some(Box::new(ExpressionParser::new().parse(parser)?))
        } else {
            None
        };
        
        let body = ClassBodyParser::new().parse(parser)?;

        Ok(ClassDeclaration {
            id,
            super_class,
            body,
        })
    }
}

pub struct ClassExpressionParser;

impl ClassExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassExpression> for ClassExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassExpression> {
        parser.assert_consume(&Token::Class, "Expected 'class'")?;

        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };
        
        let super_class = if parser.consume(&Token::Extends) {
            Some(Box::new(ExpressionParser::new().parse(parser)?))
        } else {
            None
        };

        let body = ClassBodyParser::new().parse(parser)?;
        
        Ok(ClassExpression {
            id,
            super_class,
            body,
        })
    }
}

pub struct SuperExpressionParser;

impl SuperExpressionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<SuperExpression> for SuperExpressionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<SuperExpression> {
        parser.assert_consume(&Token::Super, "Expected 'super'")?;

        Ok(SuperExpression {})
    }
}

pub struct ClassBodyParser;

impl ClassBodyParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassBody> for ClassBodyParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassBody> {
        parser.assert_consume(&Token::LeftBrace, "Expected '{' after class declaration")?;
        
        let mut body = Vec::new();
        
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Check for static block
            if parser.consume(&Token::Static) && parser.check(&Token::LeftBrace) {
                let static_block = StaticBlockParser::new().parse(parser)?;
                body.push(ClassElement::StaticBlock(static_block));
                continue;
            }
            
            // Parse method definition
            let method = MethodDefinitionParser::new().parse(parser)?;
            body.push(ClassElement::MethodDefinition(method));
        }
        
        parser.assert_consume(&Token::RightBrace, "Expected '}' after class body")?;
        
        Ok(ClassBody { body })
    }
}

pub struct MethodDefinitionParser;

impl MethodDefinitionParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<MethodDefinition> for MethodDefinitionParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<MethodDefinition> {
        // Check for static modifier
        let static_method = parser.consume(&Token::Static);
        
        // Check for method kind
        let mut kind = MethodKind::Method;
        if parser.consume(&Token::Get) {
            kind = MethodKind::Get;
        } else if parser.consume(&Token::Set) {
            kind = MethodKind::Set;
        } else if parser.consume(&Token::Constructor) {
            kind = MethodKind::Constructor;
        }
        
        // Check for async modifier
        let async_method = parser.consume(&Token::Async);
        
        // Check for generator modifier
        let generator = parser.consume(&Token::Star);
        
        // Parse the key
        let (key, computed) = if parser.consume(&Token::LeftBracket) {
            // Computed property name
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property name")?;
            (PropertyKey::Expression(Box::new(expr)), true)
        } else if parser.check(&Token::Hash) {
            // Private field or method
            parser.advance(); // Consume the '#'
            if let Token::Identifier(name) = parser.peek() {
                // Clone the name before advancing the parser
                let name_clone = name.clone();
                
                // Now advance the parser
                parser.advance();
                
                (PropertyKey::PrivateIdentifier(PrivateIdentifier { name: name_clone.into() }), false)
            } else {
                return Err(parser.error_at_current("Expected identifier after '#'"));
            }
        } else {
            // Regular identifier or literal
            match parser.peek() {
                Token::StringLiteral(_) |
                Token::NumberLiteral(_) => {
                    let literal = LiteralParser::new().parse(parser)?;
                    (PropertyKey::Literal(literal), false)
                },
                _ => {
                    let ident = IdentifierParser::new().parse(parser)?;
                    (PropertyKey::Identifier(ident), false)
                }
            }
        };
        
        // Parse the function body
        parser.assert_consume(&Token::LeftParen, "Expected '(' after method name")?;
        
        // Create a function expression for the method
        let mut func_expr = FunctionExpressionParser::new().parse(parser)?;
        func_expr.generator = generator;
        func_expr.async_function = async_method;
        
        Ok(MethodDefinition {
            key,
            value: func_expr,
            kind,
            computed,
            static_method,
        })
    }
}

pub struct StaticBlockParser;

impl StaticBlockParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<StaticBlock> for StaticBlockParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<StaticBlock> {
        let block = BlockStatementParser::new().parse(parser)?;
        
        Ok(StaticBlock { body: block })
    }
}
