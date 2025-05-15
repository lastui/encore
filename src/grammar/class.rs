use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;
use super::pattern::*;
use super::statement::*;
use super::function::*;
use super::literal::*;

pub struct ClassDeclarationNode;

impl ClassDeclarationNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassDeclaration> for ClassDeclarationNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassDeclaration> {
        parser.assert_consume(&Token::Class, "Expected 'class'")?;

        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };

        let super_class = if parser.consume(&Token::Extends) {
            Some(Box::new(ExpressionNode::new().parse(parser)?))
        } else {
            None
        };
        
        let body = ClassBodyNode::new().parse(parser)?;

        Ok(ClassDeclaration {
            id,
            super_class,
            body,
        })
    }
}

impl UnparserCombinator<ClassDeclaration> for ClassDeclarationNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ClassDeclaration) {
        unparser.write_str("class");
        
        // Write the class name if present
        if let Some(id) = &node.id {
            unparser.space();
            unparser.write_str(&id.name);
        }
        
        // Write the extends clause if present
        if let Some(super_class) = &node.super_class {
            unparser.space();
            unparser.write_str("extends");
            unparser.space();
            ExpressionNode::new().unparse(unparser, super_class);
        }
        
        unparser.space();
        
        // Write the class body
        ClassBodyNode::new().unparse(unparser, &node.body);
    }
}

pub struct ClassExpressionNode;

impl ClassExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassExpression> for ClassExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassExpression> {
        parser.assert_consume(&Token::Class, "Expected 'class'")?;

        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };
        
        let super_class = if parser.consume(&Token::Extends) {
            Some(Box::new(ExpressionNode::new().parse(parser)?))
        } else {
            None
        };

        let body = ClassBodyNode::new().parse(parser)?;
        
        Ok(ClassExpression {
            id,
            super_class,
            body,
        })
    }
}

impl UnparserCombinator<ClassExpression> for ClassExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ClassExpression) {
        unparser.write_str("class");
        
        // Write the class name if present
        if let Some(id) = &node.id {
            unparser.space();
            unparser.write_str(&id.name);
        }
        
        // Write the extends clause if present
        if let Some(super_class) = &node.super_class {
            unparser.space();
            unparser.write_str("extends");
            unparser.space();
            ExpressionNode::new().unparse(unparser, super_class);
        }
        
        unparser.space();
        
        // Write the class body
        ClassBodyNode::new().unparse(unparser, &node.body);
    }
}

pub struct SuperExpressionNode;

impl SuperExpressionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<SuperExpression> for SuperExpressionNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<SuperExpression> {
        parser.assert_consume(&Token::Super, "Expected 'super'")?;

        Ok(SuperExpression {})
    }
}

impl UnparserCombinator<SuperExpression> for SuperExpressionNode {
    fn unparse(&self, unparser: &mut Unparser, _node: &SuperExpression) {
        unparser.write_str("super");
    }
}

pub struct ClassBodyNode;

impl ClassBodyNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ClassBody> for ClassBodyNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ClassBody> {
        parser.assert_consume(&Token::LeftBrace, "Expected '{' after class declaration")?;
        
        let mut body = Vec::new();
        
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Check for static block
            if parser.consume(&Token::Static) && parser.check(&Token::LeftBrace) {
                let static_block = StaticBlockNode::new().parse(parser)?;
                body.push(ClassElement::StaticBlock(static_block));
                continue;
            }
            
            // Parse method definition
            let method = MethodDefinitionNode::new().parse(parser)?;
            body.push(ClassElement::MethodDefinition(method));
        }
        
        parser.assert_consume(&Token::RightBrace, "Expected '}' after class body")?;
        
        Ok(ClassBody { body })
    }
}

impl UnparserCombinator<ClassBody> for ClassBodyNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ClassBody) {
        unparser.write_char('{');
        
        if !node.body.is_empty() {
            unparser.newline();
            
            unparser.with_indent(|u| {
                for element in &node.body {
                    match element {
                        ClassElement::MethodDefinition(method) => {
                            MethodDefinitionNode::new().unparse(u, method);
                        },
                        ClassElement::StaticBlock(static_block) => {
                            u.write_str("static");
                            u.space();
                            StaticBlockNode::new().unparse(u, static_block);
                        }
                    }
                    u.newline();
                }
            });
        }
        
        unparser.write_char('}');
    }
}

pub struct MethodDefinitionNode;

impl MethodDefinitionNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<MethodDefinition> for MethodDefinitionNode {
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
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightBracket, "Expected ']' after computed property name")?;
            (PropertyKey::Expression(Box::new(expr)), true)
        } else if parser.check(&Token::Hash) {
            // Private field or method
            parser.advance(); // Consume the '#'
            // TODO what about string literal?
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
                    let literal = LiteralNode::new().parse(parser)?;
                    (PropertyKey::Literal(literal), false)
                },
                _ => {
                    let ident = IdentifierNode::new().parse(parser)?;
                    (PropertyKey::Identifier(ident), false)
                }
            }
        };
        
        // Parse the function body
        parser.assert_consume(&Token::LeftParen, "Expected '(' after method name")?;
        
        // Create a function expression for the method
        let mut func_expr = FunctionExpressionNode::new().parse(parser)?;
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

impl UnparserCombinator<MethodDefinition> for MethodDefinitionNode {
    fn unparse(&self, unparser: &mut Unparser, node: &MethodDefinition) {
        // Write static modifier if present
        if node.static_method {
            unparser.write_str("static");
            unparser.space();
        }
        
        // Write method kind
        match node.kind {
            MethodKind::Constructor => {
                unparser.write_str("constructor");
            },
            MethodKind::Method => {
                // For async methods
                if node.value.async_function {
                    unparser.write_str("async");
                    unparser.space();
                }
                
                // For generator methods
                if node.value.generator {
                    unparser.write_char('*');
                }
            },
            MethodKind::Get => {
                unparser.write_str("get");
                unparser.space();
            },
            MethodKind::Set => {
                unparser.write_str("set");
                unparser.space();
            }
        }
        
        // Write the method key
        if node.computed {
            unparser.write_char('[');
            match &node.key {
                PropertyKey::Expression(expr) => {
                    ExpressionNode::new().unparse(unparser, expr);
                },
                PropertyKey::Identifier(id) => {
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    match lit {
                        Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        _ => unparser.write_str("\"unknown\""),
                    }
                },
                PropertyKey::PrivateIdentifier(id) => {
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                }
            }
            unparser.write_char(']');
        } else {
            match &node.key {
                PropertyKey::Identifier(id) => {
                    unparser.write_str(&id.name);
                },
                PropertyKey::Literal(lit) => {
                    match lit {
                        Literal::StringLiteral(s) => unparser.write_str(&format!("\"{}\"", s.value)),
                        Literal::NumericLiteral(n) => unparser.write_str(&n.value.to_string()),
                        _ => unparser.write_str("\"unknown\""),
                    }
                },
                PropertyKey::Expression(_) => {
                    // This shouldn't happen for non-computed properties
                    unparser.write_str("\"error\"");
                },
                PropertyKey::PrivateIdentifier(id) => {
                    unparser.write_char('#');
                    unparser.write_str(&id.name);
                }
            }
        }
        
        // Write the method parameters and body
        unparser.write_char('(');
        
        // Write parameters
        if !node.value.params.is_empty() {
            for (i, param) in node.value.params.iter().enumerate() {
                if i > 0 {
                    unparser.write_char(',');
                    unparser.space();
                }
                PatternNode::new().unparse(unparser, param);
            }
        }
        
        unparser.write_char(')');
        unparser.space();
        
        // Write the method body
        BlockStatementNode::new().unparse(unparser, &node.value.body);
    }
}

pub struct StaticBlockNode;

impl StaticBlockNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<StaticBlock> for StaticBlockNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<StaticBlock> {
        let block = BlockStatementNode::new().parse(parser)?;
        
        Ok(StaticBlock { body: block })
    }
}

impl UnparserCombinator<StaticBlock> for StaticBlockNode {
    fn unparse(&self, unparser: &mut Unparser, node: &StaticBlock) {
        BlockStatementNode::new().unparse(unparser, &node.body);
    }
}
