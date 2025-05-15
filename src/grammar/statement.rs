use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::expression::*;
use super::declaration::*;
use super::pattern::*;
use super::class::*;
use super::object::*;

pub struct StatementNode;

impl StatementNode {
    pub fn new() -> Self {
        Self
    }

    fn determine_for_loop_type(&self, parser: &mut Parser) -> ForLoopType {
        let pos = parser.save_position();
        
        // Skip the variable declaration or pattern
        if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            parser.advance();

            while !parser.check(&Token::Semicolon) && 
                  !parser.check(&Token::In) && 
                  !parser.check(&Token::Of) && 
                  !parser.check(&Token::RightParen) && 
                  !parser.is_at_end() {
                parser.advance();
            }
        } else {
            while !parser.check(&Token::Semicolon) && 
                  !parser.check(&Token::In) && 
                  !parser.check(&Token::Of) && 
                  !parser.check(&Token::RightParen) && 
                  !parser.is_at_end() {
                parser.advance();
            }
        }

        let loop_type = match parser.peek() {
            Token::In => ForLoopType::ForIn,
            Token::Of => ForLoopType::ForOf,
            _ => ForLoopType::Standard,
        };
        
        // Restore position
        parser.restore_position(pos);
        
        loop_type
    }

}

// Enum to represent the different types of for loops
enum ForLoopType {
    Standard,  // for (init; test; update)
    ForIn,     // for (left in right)
    ForOf,     // for (left of right)
}

impl ParserCombinator<Statement> for StatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Statement> {
        match parser.peek() {
            // Special case for object literals at the start of a statement
            Token::LeftBrace => {
                // Try to parse as object expression first
                let pos = parser.save_position();
                
                // Attempt to parse as an object literal
                match ObjectExpressionNode::new().parse(parser) {
                    Ok(obj_expr) => {

                        // Successfully parsed as object expression
                        // Consume the semicolon if present
                        parser.consume(&Token::Semicolon);
                        
                        return Ok(Statement::ExpressionStatement(ExpressionStatement {
                            expression: Box::new(Expression::ObjectExpression(obj_expr)),
                        }));
                    },
                    Err(_) => {
                        // Failed to parse as object expression, restore position and try as block statement
                        parser.restore_position(pos);
                        return BlockStatementNode::new().parse(parser).map(Statement::BlockStatement);
                    }
                }
            },
            Token::Var |
            Token::Let |
            Token::Const => {
                VariableDeclarationNode::new().parse(parser).map(|decl| 
                    Statement::Declaration(Declaration::VariableDeclaration(decl))
                )
            },
            Token::Async => {
                // Check if this is an async function declaration
                let pos = parser.save_position();
                parser.advance(); // Skip 'async'
                
                if parser.check(&Token::Function) {
                    // This is an async function declaration or expression
                    parser.restore_position(pos);
                    
                    // Try to parse as function declaration first
                    let pos2 = parser.save_position();
                    parser.advance(); // Skip 'async'
                    parser.advance(); // Skip 'function'
                    
                    // Check for generator function
                    let _is_generator = parser.consume(&Token::Star);
                    
                    // If the next token is an identifier, this is a function declaration
                    if let Token::Identifier(_) = parser.peek() {
                        parser.restore_position(pos);
                        FunctionDeclarationParser::new().parse(parser).map(|decl| 
                            Statement::Declaration(Declaration::FunctionDeclaration(decl))
                        )
                    } else {
                        // Otherwise, it's a function expression statement
                        parser.restore_position(pos);
                        ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
                    }
                } else {
                    // Not a function, treat as regular expression statement
                    parser.restore_position(pos);
                    ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            Token::Function => {
                // Check if this is a function declaration (has an identifier)
                let pos = parser.save_position();
                parser.advance(); // Skip 'function'
                
                // Check for generator function
                let _is_generator = parser.consume(&Token::Star);
                
                // If the next token is an identifier, this is a function declaration
                if let Token::Identifier(_) = parser.peek() {
                    parser.restore_position(pos);
                    FunctionDeclarationParser::new().parse(parser).map(|decl| Statement::Declaration(Declaration::FunctionDeclaration(decl)))
                } else {
                    // Otherwise, it's a function expression statement
                    parser.restore_position(pos);
                    ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            Token::Class => {
                // Check if this is a class declaration (has an identifier)
                let pos = parser.save_position();
                parser.advance(); // Skip 'class'
                
                // If the next token is an identifier, this is a class declaration
                if let Token::Identifier(_) = parser.peek() {
                    parser.restore_position(pos);
                    ClassDeclarationNode::new().parse(parser).map(|decl| Statement::Declaration(Declaration::ClassDeclaration(decl)))
                } else {
                    // Otherwise, it's a class expression statement
                    parser.restore_position(pos);
                    ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            Token::Import => {
                ImportDeclarationParser::new().parse(parser).map(|decl| 
                    Statement::Declaration(Declaration::ImportDeclaration(decl))
                )
            },
            Token::Export => {
                ExportDeclarationParser::new().parse(parser).map(|decl| {
                    match decl {
                        ExportDeclaration::Named(named) => Statement::Declaration(Declaration::ExportNamedDeclaration(named)),
                        ExportDeclaration::Default(default) => Statement::Declaration(Declaration::ExportDefaultDeclaration(default)),
                        ExportDeclaration::All(all) => Statement::Declaration(Declaration::ExportAllDeclaration(all)),
                    }
                })
            },
            Token::If => {
                IfStatementNode::new().parse(parser).map(Statement::IfStatement)
            },
            Token::Switch => {
                SwitchStatementNode::new().parse(parser).map(Statement::SwitchStatement)
            },
            Token::For => {
                let pos = parser.save_position();
                
                // Consume the 'for' token
                parser.advance();
                
                // Expect opening parenthesis
                if !parser.consume(&Token::LeftParen) {
                    parser.restore_position(pos);
                    return Err(parser.error_at_current("Expected '(' after 'for'"));
                }
                
                // Look ahead to determine the type of for loop
                let loop_type = self.determine_for_loop_type(parser);
                
                // Restore position to start parsing the full statement
                parser.restore_position(pos);
                
                match loop_type {
                    ForLoopType::Standard => {
                        ForStatementNode::new().parse(parser).map(Statement::ForStatement)
                    },
                    ForLoopType::ForIn => {
                        ForInStatementNode::new().parse(parser).map(Statement::ForInStatement)
                    },
                    ForLoopType::ForOf => {
                        ForOfStatementNode::new().parse(parser).map(Statement::ForOfStatement)
                    },
                }
            },
            Token::While => {
                WhileStatementNode::new().parse(parser).map(Statement::WhileStatement)
            },
            Token::Do => {
                DoWhileStatementNode::new().parse(parser).map(Statement::DoWhileStatement)
            },
            Token::Try => {
                TryStatementNode::new().parse(parser).map(Statement::TryStatement)
            },
            Token::With => {
                WithStatementNode::new().parse(parser).map(Statement::WithStatement)
            },
            Token::Throw => {
                ThrowStatementNode::new().parse(parser).map(Statement::ThrowStatement)
            },
            Token::Return => {
                ReturnStatementNode::new().parse(parser).map(Statement::ReturnStatement)
            },
            Token::Break => {
                BreakStatementNode::new().parse(parser).map(Statement::BreakStatement)
            },
            Token::Continue => {
                ContinueStatementNode::new().parse(parser).map(Statement::ContinueStatement)
            },
            Token::Debugger => {
                parser.advance(); // Consume 'debugger'
                parser.consume(&Token::Semicolon); // Optional semicolon
                Ok(Statement::DebuggerStatement)
            },
            Token::Semicolon => {
                parser.advance(); // Consume ';'
                Ok(Statement::EmptyStatement)
            },
            // Check for labeled statements (identifier followed by colon)
            Token::Identifier(_) => {
                let pos = parser.save_position();
                let ident = IdentifierNode::new().parse(parser)?;
                
                if parser.consume(&Token::Colon) {
                    // This is a labeled statement
                    let body = Box::new(self.parse(parser)?);
                    Ok(Statement::LabeledStatement(LabeledStatement { label: ident, body }))
                } else {
                    // Not a labeled statement, restore position and parse as expression statement
                    parser.restore_position(pos);
                    ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            // Default to expression statement
            _ => {
                ExpressionStatementNode::new().parse(parser).map(Statement::ExpressionStatement)
            }
        }
    }
}

/// Parser for block statements
pub struct BlockStatementNode;

impl BlockStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<BlockStatement> for BlockStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<BlockStatement> {
        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of block statement")?;
        
        let mut body = Vec::new();
        
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Parse a statement
            let statement = StatementNode::new().parse(parser)?;
            body.push(statement);
        }
        
        parser.assert_consume(&Token::RightBrace, "Expected '}' at the end of block statement")?;
        
        Ok(BlockStatement { body })
    }
}

impl UnparserCombinator<BlockStatement> for BlockStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &BlockStatement) {
        unparser.write_char('{');
        unparser.newline();
        
        if !node.body.is_empty() {
            unparser.with_indent(|u| {
                for stmt in &node.body {
                    StatementNode::new().unparse(u, stmt);
                    u.newline();
                }
            });
        }
        
        unparser.write_char('}');
    }
}

/// Parser for expression statements
pub struct ExpressionStatementNode;

impl ExpressionStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ExpressionStatement> for ExpressionStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExpressionStatement> {
        // Check for directive prologue (string literals that might be "use strict")
        if let Token::StringLiteral(_) = parser.peek() {
            let pos = parser.save_position();
            let expr = ExpressionNode::new().parse(parser)?;
            
            // If this is followed by a semicolon or end of block, it's a directive
            if parser.check(&Token::Semicolon) || parser.check(&Token::RightBrace) || parser.is_at_end() {
                // Consume the semicolon if present
                parser.consume(&Token::Semicolon);
                    
                /*
                // Check if this is "use strict"
                if let Expression::Literal(Literal::StringLiteral(StringLiteral { value })) = &expr {
                    if value == "use strict" {
                        // Set strict mode
                        parser.set_strict_mode(true);
                    }
                }*/
                
                return Ok(ExpressionStatement {
                    expression: Box::new(expr),
                });
            }
            
            // Not a directive, restore position and continue with normal parsing
            parser.restore_position(pos);
        }
        
        // Special case for object literals at the start of a statement
        if parser.check(&Token::LeftBrace) {
            let pos = parser.save_position();
            
            // Try to parse as object expression
            match ObjectExpressionNode::new().parse(parser) {
                Ok(obj_expr) => {
                    // Successfully parsed as object expression
                    // Consume the semicolon if present
                    parser.consume(&Token::Semicolon);
                    
                    return Ok(ExpressionStatement {
                        expression: Box::new(Expression::ObjectExpression(obj_expr)),
                    });
                },
                Err(_) => {
                    // Failed to parse as object expression, restore position
                    parser.restore_position(pos);
                    // Will fall through to regular expression parsing
                }
            }
        }
        
        // Regular expression statement parsing
        let expr = ExpressionNode::new().parse(parser)?;
        
        // Consume the semicolon if present (ASI rules apply)
        // In JavaScript, semicolons are optional in many cases due to Automatic Semicolon Insertion (ASI)
        if !parser.previous_line_terminator() && 
           !parser.check(&Token::RightBrace) && 
           !parser.is_at_end() {
            parser.assert_consume(&Token::Semicolon, "Expected ';' after expression statement")?;
        } else {
            // Semicolon is optional if:
            // 1. There's a line terminator after the expression
            // 2. The next token is a closing brace
            // 3. We're at the end of the input
            parser.consume(&Token::Semicolon);
        }
        
        Ok(ExpressionStatement {
            expression: Box::new(expr),
        })
    }
}

/// Parser for if statements
pub struct IfStatementNode;

impl IfStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<IfStatement> for IfStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<IfStatement> {
        parser.assert_consume(&Token::If, "Expected 'if'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'if'")?;
        
        let test = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after if condition")?;
        
        // Check if the consequent starts with a left brace
        let consequent = if parser.check(&Token::LeftBrace) {
            // Force parsing as a block statement
            Box::new(BlockStatementNode::new().parse(parser).map(Statement::BlockStatement)?)
        } else {
            // For other statement types, use the general statement parser
            Box::new(StatementNode::new().parse(parser)?)
        };
        
        let alternate = if parser.consume(&Token::Else) {
            Some(Box::new(StatementNode::new().parse(parser)?))
        } else {
            None
        };
        
        Ok(IfStatement {
            test,
            consequent,
            alternate,
        })
    }
}


/// Parser for switch statements
pub struct SwitchStatementNode;

impl SwitchStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<SwitchStatement> for SwitchStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<SwitchStatement> {
        parser.assert_consume(&Token::Switch, "Expected 'switch'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'switch'")?;
        
        let discriminant = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after switch expression")?;
        parser.assert_consume(&Token::LeftBrace, "Expected '{' to start switch body")?;
        
        let cases = parser.with_context(LexicalContext::SwitchBody, |p| {

            let mut result = Vec::new();
        
            while !p.check(&Token::RightBrace) && !p.is_at_end() {
                if p.consume(&Token::Case) {
                    // Case clause
                    let test = Some(Box::new(ExpressionNode::new().parse(p)?));
                    p.assert_consume(&Token::Colon, "Expected ':' after case value")?;
                    
                    let mut consequent = Vec::new();
                    while !p.check(&Token::Case) && 
                          !p.check(&Token::Default) && 
                          !p.check(&Token::RightBrace) && 
                          !p.is_at_end() {
                        consequent.push(StatementNode::new().parse(p)?);
                    }
                    
                    result.push(SwitchCase { test, consequent });
                } else if p.consume(&Token::Default) {
                    // Default clause
                    p.assert_consume(&Token::Colon, "Expected ':' after 'default'")?;
                    
                    let mut consequent = Vec::new();
                    while !p.check(&Token::Case) && 
                          !p.check(&Token::Default) && 
                          !p.check(&Token::RightBrace) && 
                          !p.is_at_end() {
                        consequent.push(StatementNode::new().parse(p)?);
                    }
                    
                    result.push(SwitchCase { test: None, consequent });
                } else {
                    return Err(p.error_at_current("Expected 'case' or 'default' in switch statement"));
                }
            }
            
            p.assert_consume(&Token::RightBrace, "Expected '}' to end switch statement")?;
            
            Ok(result)
        })?;

        
        Ok(SwitchStatement {
            discriminant,
            cases,
        })
    }
}

/// Parser for while statements
pub struct WhileStatementNode;

impl WhileStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<WhileStatement> for WhileStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<WhileStatement> {
        parser.assert_consume(&Token::While, "Expected 'while'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after while condition")?;
            
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            // Check if the body starts with a left brace
            if p.check(&Token::LeftBrace) {
                // Force parsing as a block statement
                BlockStatementNode::new().parse(p).map(Statement::BlockStatement)
            } else {
                // For other statement types, use the general statement parser
                StatementNode::new().parse(p)
            }
        })?;

        Ok(WhileStatement {
            test,
            body: Box::new(body),
        })
    }
}

/// Parser for do-while statements
pub struct DoWhileStatementNode;

impl DoWhileStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<DoWhileStatement> for DoWhileStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<DoWhileStatement> {
        parser.assert_consume(&Token::Do, "Expected 'do'")?;
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            // Check if the body starts with a left brace
            if p.check(&Token::LeftBrace) {
                // Force parsing as a block statement
                BlockStatementNode::new().parse(p).map(Statement::BlockStatement)
            } else {
                // For other statement types, use the general statement parser
                StatementNode::new().parse(p)
            }
        })?;

        parser.assert_consume(&Token::While, "Expected 'while' after do block")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after while condition")?;
        parser.assert_consume(&Token::Semicolon, "Expected ';' after while condition")?;

        Ok(DoWhileStatement {
            body: Box::new(body),
            test,
        })
    }
}

/// Parser for for statements
pub struct ForStatementNode;

impl ForStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ForStatement> for ForStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse initialization
        let init = if parser.consume(&Token::Semicolon) {
            None
        } else if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationNode::new().parse(parser)?;
            
            // Check if this is a for-in or for-of loop
            if parser.check(&Token::In) || parser.check(&Token::Of) {
                return Err(parser.error_at_current("Expected ';' after for initialization"));
            }
            
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for initialization")?;
            Some(ForInit::VariableDeclaration(decl))
        } else {
            // Expression
            let expr = ExpressionNode::new().parse(parser)?;
            
            // Check if this is a for-in or for-of loop
            if parser.check(&Token::In) || parser.check(&Token::Of) {
                return Err(parser.error_at_current("Expected ';' after for initialization"));
            }
            
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for initialization")?;
            Some(ForInit::Expression(Box::new(expr)))
        };
        
        // Parse condition
        let test = if parser.consume(&Token::Semicolon) {
            None
        } else {
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for condition")?;
            Some(Box::new(expr))
        };
        
        // Parse update
        let update = if parser.consume(&Token::RightParen) {
            None
        } else {
            let expr = ExpressionNode::new().parse(parser)?;
            parser.assert_consume(&Token::RightParen, "Expected ')' after for clauses")?;
            Some(Box::new(expr))
        };
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            // Check if the body starts with a left brace
            if p.check(&Token::LeftBrace) {
                // Force parsing as a block statement
                BlockStatementNode::new().parse(p).map(Statement::BlockStatement)
            } else {
                // For other statement types, use the general statement parser
                StatementNode::new().parse(p)
            }
        })?;
        
        Ok(ForStatement {
            init,
            test,
            update,
            body: Box::new(body),
        })
    }
}


/// Parser for for-in statements
pub struct ForInStatementNode;

impl ForInStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ForInStatement> for ForInStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForInStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse left side (variable declaration or pattern)
        let left = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationNode::new().parse(parser)?;
            ForInOf::VariableDeclaration(decl)
        } else {
            // Pattern
            let pattern = PatternNode::new().parse(parser)?;
            ForInOf::Pattern(pattern)
        };
        
        // Check for 'in' keyword - fail early if not found
        if !parser.check(&Token::In) {
            return Err(parser.error_at_current("Expected 'in' in for-in statement"));
        }
        
        // Expect 'in' keyword
        parser.assert_consume(&Token::In, "Expected 'in' in for-in statement")?;
        
        // Parse right side (expression)
        let right = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after for-in clauses")?;
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            // Check if the body starts with a left brace
            if p.check(&Token::LeftBrace) {
                // Force parsing as a block statement
                BlockStatementNode::new().parse(p).map(Statement::BlockStatement)
            } else {
                // For other statement types, use the general statement parser
                StatementNode::new().parse(p)
            }
        })?;
        
        Ok(ForInStatement {
            left,
            right,
            body: Box::new(body),
        })
    }
}

impl UnparserCombinator<ForInStatement> for ForInStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ForInStatement) {
        unparser.write_str("for");
        unparser.space();
        unparser.write_char('(');
        
        match &node.left {
            ForInOf::VariableDeclaration(decl) => {
                // Special handling for variable declarations in for-in loops
                // Write the variable kind (var, let, const)
                match decl.kind {
                    VariableKind::Var => unparser.write_str("var"),
                    VariableKind::Let => unparser.write_str("let"),
                    VariableKind::Const => unparser.write_str("const"),
                }
                
                unparser.write_char(' ');
                
                // Write the declarations without semicolon
                if !decl.declarations.is_empty() {
                    // First declaration
                    VariableDeclaratorNode::new().unparse(unparser, &decl.declarations[0]);
                    
                    // Remaining declarations
                    for d in &decl.declarations[1..] {
                        unparser.write_char(',');
                        unparser.space();
                        VariableDeclaratorNode::new().unparse(unparser, d);
                    }
                }
                // No semicolon here!
            },
            ForInOf::Pattern(pattern) => {
                PatternNode::new().unparse(unparser, pattern);
            }
        }
        
        unparser.write_char(' ');
        unparser.write_str("in");
        unparser.write_char(' ');
        
        ExpressionNode::new().unparse(unparser, &node.right);
        
        unparser.write_char(')');
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                unparser.space();
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
            }
        }
    }
}


/// Parser for for-of statements
pub struct ForOfStatementNode;

impl ForOfStatementNode {
    pub fn new() -> Self {
        Self
    }
}

/// Parser for for-of statements
impl ParserCombinator<ForOfStatement> for ForOfStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForOfStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        
        // Check for 'await' (for await of)
        let await_token = parser.consume(&Token::Await);
        
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse left side (variable declaration or pattern)
        let left = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationNode::new().parse(parser)?;
            ForInOf::VariableDeclaration(decl)
        } else {
            // Pattern
            let pattern = PatternNode::new().parse(parser)?;
            ForInOf::Pattern(pattern)
        };
        
        // Check for 'of' keyword - fail early if not found
        if !parser.check(&Token::Of) {
            return Err(parser.error_at_current("Expected 'of' in for-of statement"));
        }
        
        // Expect 'of' keyword
        parser.assert_consume(&Token::Of, "Expected 'of' in for-of statement")?;
        
        // Parse right side (expression)
        let right = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after for-of clauses")?;

        // Parse the body with special handling for block statements
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            // Check if the body starts with a left brace
            if p.check(&Token::LeftBrace) {
                // Force parsing as a block statement
                BlockStatementNode::new().parse(p).map(Statement::BlockStatement)
            } else {
                // For other statement types, use the general statement parser
                StatementNode::new().parse(p)
            }
        })?;
        
        Ok(ForOfStatement {
            left,
            right,
            body: Box::new(body),
            await_token,
        })
    }
}


impl UnparserCombinator<ForOfStatement> for ForOfStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ForOfStatement) {
        unparser.write_str("for");
        
        if node.await_token {
            unparser.write_char(' ');
            unparser.write_str("await");
        }
        
        unparser.space();
        unparser.write_char('(');
        
        match &node.left {
            ForInOf::VariableDeclaration(decl) => {
                // Special handling for variable declarations in for-of loops
                // Write the variable kind (var, let, const)
                match decl.kind {
                    VariableKind::Var => unparser.write_str("var"),
                    VariableKind::Let => unparser.write_str("let"),
                    VariableKind::Const => unparser.write_str("const"),
                }
                
                unparser.write_char(' ');
                
                // Write the declarations without semicolon
                if !decl.declarations.is_empty() {
                    // First declaration
                    VariableDeclaratorNode::new().unparse(unparser, &decl.declarations[0]);
                    
                    // Remaining declarations
                    for d in &decl.declarations[1..] {
                        unparser.write_char(',');
                        unparser.space();
                        VariableDeclaratorNode::new().unparse(unparser, d);
                    }
                }
                // No semicolon here!
            },
            ForInOf::Pattern(pattern) => {
                PatternNode::new().unparse(unparser, pattern);
            }
        }
        
        unparser.write_char(' ');
        unparser.write_str("of");
        unparser.write_char(' ');
        
        ExpressionNode::new().unparse(unparser, &node.right);
        
        unparser.write_char(')');
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                unparser.space();
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
            }
        }
    }
}


/// Parser for break statements
pub struct BreakStatementNode;

impl BreakStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<BreakStatement> for BreakStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<BreakStatement> {
        parser.assert_consume(&Token::Break, "Expected 'break'")?;
        
        // Check if we're in a loop or switch
        if !parser.is_in_loop_body() && !parser.is_in_switch() {
            return Err(parser.error_at_current("'break' statement can only be used within a loop or switch statement"));
        }
        
        // Check for label
        let label = if !parser.previous_line_terminator() && matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(BreakStatement { label })
    }
}

/// Parser for continue statements
pub struct ContinueStatementNode;

impl ContinueStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ContinueStatement> for ContinueStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ContinueStatement> {
        parser.assert_consume(&Token::Continue, "Expected 'continue'")?;
        
        // Check if we're in a loop
        if !parser.is_in_loop_body() {
            return Err(parser.error_at_current("'continue' statement can only be used within a loop"));
        }
        
        // Check for label
        let label = if !parser.previous_line_terminator() && matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(ContinueStatement { label })
    }
}

impl UnparserCombinator<ContinueStatement> for ContinueStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ContinueStatement) {
        unparser.write_str("continue");
        
        if let Some(label) = &node.label {
            unparser.space();
            unparser.write_str(&label.name);
        }
        
        unparser.write_char(';');
    }
}

/// Parser for return statements
pub struct ReturnStatementNode;

impl ReturnStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ReturnStatement> for ReturnStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ReturnStatement> {
        parser.assert_consume(&Token::Return, "Expected 'return'")?;
        
        // Check if we're in a function
        if !parser.is_in_function() {
            return Err(parser.error_at_current("'return' statement can only be used within a function"));
        }
        
        // Check for return value
        let argument = if parser.previous_line_terminator() || 
                         parser.check(&Token::Semicolon) || 
                         parser.check(&Token::RightBrace) || 
                         parser.is_at_end() {
            None
        } else {
            Some(Box::new(ExpressionNode::new().parse(parser)?))
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(ReturnStatement { argument })
    }
}

impl UnparserCombinator<ReturnStatement> for ReturnStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ReturnStatement) {
        unparser.write_str("return");
        
        if let Some(argument) = &node.argument {
            unparser.write_char(' ');
            ExpressionNode::new().unparse(unparser, argument);
        }
        
        unparser.write_char(';');
    }
}

/// Parser for with statements
pub struct WithStatementNode;

impl WithStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<WithStatement> for WithStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<WithStatement> {
        parser.assert_consume(&Token::With, "Expected 'with'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'with'")?;
        
        let object = Box::new(ExpressionNode::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after with object")?;
        
        let body = Box::new(StatementNode::new().parse(parser)?);
        
        Ok(WithStatement {
            object,
            body,
        })
    }
}

impl UnparserCombinator<WithStatement> for WithStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &WithStatement) {
        unparser.write_str("with");
        unparser.write_char(' ');
        unparser.write_char('(');
        ExpressionNode::new().unparse(unparser, &node.object);
        unparser.write_char(')');
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                unparser.space();
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
            }
        }
    }
}

/// Parser for throw statements
pub struct ThrowStatementNode;

impl ThrowStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ThrowStatement> for ThrowStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ThrowStatement> {
        parser.assert_consume(&Token::Throw, "Expected 'throw'")?;
        
        if parser.previous_line_terminator() {
            return Err(parser.error_at_current("Line terminator not allowed after 'throw'"));
        }
        
        let argument = Box::new(ExpressionNode::new().parse(parser)?);
    
        parser.consume(&Token::Semicolon);
        
        Ok(ThrowStatement { argument })
    }
}

impl UnparserCombinator<ThrowStatement> for ThrowStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ThrowStatement) {
        unparser.write_str("throw");
        unparser.write_char(' ');
        ExpressionNode::new().unparse(unparser, &node.argument);
        unparser.write_char(';');
    }
}

/// Parser for try statements
pub struct TryStatementNode;

impl TryStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<TryStatement> for TryStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<TryStatement> {
        parser.assert_consume(&Token::Try, "Expected 'try'")?;
        
        let block = BlockStatementNode::new().parse(parser)?;
        
        // Parse catch clause if present
        let handler = if parser.consume(&Token::Catch) {
            // Parse parameter if present
            let param = if parser.consume(&Token::LeftParen) {
                let pattern = PatternNode::new().parse(parser)?;
                parser.assert_consume(&Token::RightParen, "Expected ')' after catch parameter")?;
                Some(pattern)
            } else {
                None
            };
            
            let body = BlockStatementNode::new().parse(parser)?;
            
            Some(CatchClause {
                param,
                body,
            })

        } else {
            None
        };
        
        // Parse finally clause if present
        let finalizer = if parser.consume(&Token::Finally) {
            Some(BlockStatementNode::new().parse(parser)?)
        } else {
            None
        };
        
        // Either catch or finally must be present
        if handler.is_none() && finalizer.is_none() {
            return Err(parser.error_at_current("Missing catch or finally after try"));
        }
        
        Ok(TryStatement {
            block,
            handler,
            finalizer,
        })
    }
}

impl UnparserCombinator<TryStatement> for TryStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &TryStatement) {
        unparser.write_str("try");
        unparser.space();
        BlockStatementNode::new().unparse(unparser, &node.block);
        
        // Handle catch clause if present
        if let Some(handler) = &node.handler {
            unparser.space();
            unparser.write_str("catch");
            
            // Handle catch parameter if present
            if let Some(param) = &handler.param {
                unparser.space();
                unparser.write_char('(');
                PatternNode::new().unparse(unparser, param);
                unparser.write_char(')');
            }
            
            unparser.space();
            BlockStatementNode::new().unparse(unparser, &handler.body);
        }
        
        // Handle finally clause if present
        if let Some(finalizer) = &node.finalizer {
            unparser.space();
            unparser.write_str("finally");
            unparser.space();
            BlockStatementNode::new().unparse(unparser, finalizer);
        }
    }
}

/// Parser for labeled statements
pub struct LabeledStatementNode;

impl LabeledStatementNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<LabeledStatement> for LabeledStatementNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<LabeledStatement> {
        let label = IdentifierNode::new().parse(parser)?;
        
        parser.assert_consume(&Token::Colon, "Expected ':' after label")?;
        
        // Add label to context
        //parser.add_label(label.name.clone());
        
        let body = Box::new(StatementNode::new().parse(parser)?);
        
        // Remove label from context
        //parser.remove_label(&label.name);
        
        Ok(LabeledStatement {
            label,
            body,
        })
    }
}

impl UnparserCombinator<LabeledStatement> for LabeledStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &LabeledStatement) {
        unparser.write_str(&node.label.name);
        unparser.write_char(':');
        unparser.space();
        StatementNode::new().unparse(unparser, &node.body);
    }
}

// Main statement unparser
impl UnparserCombinator<Statement> for StatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &Statement) {
        match node {
            Statement::BlockStatement(stmt) => BlockStatementNode::new().unparse(unparser, stmt),
            Statement::BreakStatement(stmt) => BreakStatementNode::new().unparse(unparser, stmt),
            Statement::ContinueStatement(stmt) => ContinueStatementNode::new().unparse(unparser, stmt),
            Statement::DebuggerStatement => {
                // TODO its own unparser
                unparser.write_str("debugger");
                unparser.write_char(';');
            },
            Statement::DoWhileStatement(stmt) => DoWhileStatementNode::new().unparse(unparser, stmt),
            //Statement::EmptyStatement => unparser.write_char(';'),
            Statement::EmptyStatement => {},
            Statement::ExpressionStatement(stmt) => ExpressionStatementNode::new().unparse(unparser, stmt),
            Statement::ForStatement(stmt) => ForStatementNode::new().unparse(unparser, stmt),
            Statement::ForInStatement(stmt) => ForInStatementNode::new().unparse(unparser, stmt),
            Statement::ForOfStatement(stmt) => ForOfStatementNode::new().unparse(unparser, stmt),
            Statement::IfStatement(stmt) => IfStatementNode::new().unparse(unparser, stmt),
            Statement::LabeledStatement(stmt) => LabeledStatementNode::new().unparse(unparser, stmt),
            Statement::ReturnStatement(stmt) => ReturnStatementNode::new().unparse(unparser, stmt),
            Statement::SwitchStatement(stmt) => SwitchStatementNode::new().unparse(unparser, stmt),
            Statement::ThrowStatement(stmt) => ThrowStatementNode::new().unparse(unparser, stmt),
            Statement::TryStatement(stmt) => TryStatementNode::new().unparse(unparser, stmt),
            Statement::WhileStatement(stmt) => WhileStatementNode::new().unparse(unparser, stmt),
            Statement::WithStatement(stmt) => WithStatementNode::new().unparse(unparser, stmt),
            Statement::Declaration(decl) => {
                match decl {
                    Declaration::ClassDeclaration(decl) => ClassDeclarationNode::new().unparse(unparser, decl),
                    Declaration::FunctionDeclaration(decl) => FunctionDeclarationParser::new().unparse(unparser, decl),
                    Declaration::VariableDeclaration(decl) => VariableDeclarationNode::new().unparse(unparser, decl),
                    Declaration::ImportDeclaration(decl) => ImportDeclarationParser::new().unparse(unparser, decl),
                    Declaration::ExportNamedDeclaration(decl) => ExportNamedDeclarationParser::new().unparse(unparser, decl),
                    Declaration::ExportDefaultDeclaration(decl) => ExportDefaultDeclarationParser::new().unparse(unparser, decl),
                    Declaration::ExportAllDeclaration(decl) => ExportAllDeclarationParser::new().unparse(unparser, decl),
                }
            }
        }
    }
}

// Expression statement unparser
impl UnparserCombinator<ExpressionStatement> for ExpressionStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ExpressionStatement) {
        ExpressionNode::new().unparse(unparser, &node.expression);
        unparser.write_char(';');
    }
}

// If statement unparser
impl UnparserCombinator<IfStatement> for IfStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &IfStatement) {
        unparser.write_str("if");
        unparser.space();
        unparser.write_char('(');
        ExpressionNode::new().unparse(unparser, &node.test);
        unparser.write_char(')');
        
        // Handle consequent
        match &*node.consequent {
            Statement::BlockStatement(block) => {
                unparser.space();
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.consequent);
                });
            }
        }
        
        // Handle alternate (else branch)
        if let Some(alt) = &node.alternate {
            unparser.space();
            unparser.write_str("else");
            
            match &**alt {
                Statement::IfStatement(_) => {
                    // For else if, keep on same line
                    unparser.space();
                    StatementNode::new().unparse(unparser, alt);
                },
                Statement::BlockStatement(block) => {
                    unparser.space();
                    BlockStatementNode::new().unparse(unparser, block);
                },
                _ => {
                    unparser.newline();
                    unparser.with_indent(|u| {
                        StatementNode::new().unparse(u, alt);
                    });
                }
            }
        }
    }
}

// Switch statement unparser
impl UnparserCombinator<SwitchStatement> for SwitchStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &SwitchStatement) {
        unparser.write_str("switch");
        unparser.space();
        unparser.write_char('(');
        ExpressionNode::new().unparse(unparser, &node.discriminant);
        unparser.write_char(')');
        unparser.space();
        unparser.write_char('{');
        unparser.newline();
        
        for case in &node.cases {
            if let Some(test) = &case.test {
                unparser.write_str("case");
                unparser.write_char(' ');
                ExpressionNode::new().unparse(unparser, test);
                unparser.write_char(':');
            } else {
                unparser.write_str("default:");
            }
            
            if !case.consequent.is_empty() {
                unparser.newline();
                
                unparser.with_indent(|u| {
                    for stmt in &case.consequent {
                        StatementNode::new().unparse(u, stmt);
                        u.newline();
                    }
                });
            } else {
                unparser.newline();
            }
        }
        
        unparser.write_char('}');
    }
}

// While statement unparser
impl UnparserCombinator<WhileStatement> for WhileStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &WhileStatement) {
        unparser.write_str("while");
        unparser.space();
        unparser.write_char('(');
        ExpressionNode::new().unparse(unparser, &node.test);
        unparser.write_char(')');
        unparser.space();
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
            }
        }
    }
}

// Do-while statement unparser
impl UnparserCombinator<DoWhileStatement> for DoWhileStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &DoWhileStatement) {
        unparser.write_str("do");
        unparser.space();
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
                unparser.newline();
            }
        }
        
        unparser.space();
        unparser.write_str("while");
        unparser.space();
        unparser.write_char('(');
        ExpressionNode::new().unparse(unparser, &node.test);
        unparser.write_str(");");
    }
}

// For statement unparser
impl UnparserCombinator<ForStatement> for ForStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &ForStatement) {
        unparser.write_str("for");
        unparser.space();
        unparser.write_char('(');
        
        // Initialization
        if let Some(init) = &node.init {
            match init {
                ForInit::VariableDeclaration(decl) => {
                    VariableDeclarationNode::new().unparse(unparser, decl);
                },
                ForInit::Expression(expr) => {
                    ExpressionNode::new().unparse(unparser, expr);
                    unparser.write_char(';');
                }
            }
        } else {
            unparser.write_char(';');
        }
        
        // Test condition
        unparser.space();
        if let Some(test) = &node.test {
            ExpressionNode::new().unparse(unparser, test);
        }
        unparser.write_char(';');
        
        // Update expression
        unparser.space();
        if let Some(update) = &node.update {
            ExpressionNode::new().unparse(unparser, update);
        }
        
        unparser.write_char(')');
        unparser.space();
        
        match &*node.body {
            Statement::BlockStatement(block) => {
                BlockStatementNode::new().unparse(unparser, block);
            },
            _ => {
                unparser.newline();
                unparser.with_indent(|u| {
                    StatementNode::new().unparse(u, &node.body);
                });
            }
        }
    }
}


// Break statement unparser
impl UnparserCombinator<BreakStatement> for BreakStatementNode {
    fn unparse(&self, unparser: &mut Unparser, node: &BreakStatement) {
        unparser.write_str("break");
        
        if let Some(label) = &node.label {
            unparser.space();
            unparser.write_str(&label.name);
        }
        
        unparser.write_char(';');
    }
}
