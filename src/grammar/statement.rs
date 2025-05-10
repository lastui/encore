use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::expression::*;
use super::declaration::*;
use super::pattern::*;
use super::class::*;
use super::object::*;

/// Parser for JavaScript statements
pub struct StatementParser;

impl StatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Statement> for StatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Statement> {
        match parser.peek() {
            // Special case for object literals at the start of a statement
            Token::LeftBrace => {
                // Try to parse as object expression first
                let pos = parser.save_position();
                
                // Attempt to parse as an object literal
                match ObjectExpressionParser::new().parse(parser) {
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
                        return BlockStatementParser::new().parse(parser).map(Statement::BlockStatement);
                    }
                }
            },
            Token::Var |
            Token::Let |
            Token::Const => {
                VariableDeclarationParser::new().parse(parser).map(|decl| 
                    Statement::Declaration(Declaration::VariableDeclaration(decl))
                )
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
                    ExpressionStatementParser::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            Token::Class => {
                // Check if this is a class declaration (has an identifier)
                let pos = parser.save_position();
                parser.advance(); // Skip 'class'
                
                // If the next token is an identifier, this is a class declaration
                if let Token::Identifier(_) = parser.peek() {
                    parser.restore_position(pos);
                    ClassDeclarationParser::new().parse(parser).map(|decl| Statement::Declaration(Declaration::ClassDeclaration(decl)))
                } else {
                    // Otherwise, it's a class expression statement
                    parser.restore_position(pos);
                    ExpressionStatementParser::new().parse(parser).map(Statement::ExpressionStatement)
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
                IfStatementParser::new().parse(parser).map(Statement::IfStatement)
            },
            Token::Switch => {
                SwitchStatementParser::new().parse(parser).map(Statement::SwitchStatement)
            },
            Token::For => {
                // Try to parse as for statement
                let pos = parser.save_position();
                match ForStatementParser::new().parse(parser) {
                    Ok(stmt) => Ok(Statement::ForStatement(stmt)),
                    Err(_) => {
                        // Try to parse as for-in statement
                        parser.restore_position(pos);
                        match ForInStatementParser::new().parse(parser) {
                            Ok(stmt) => Ok(Statement::ForInStatement(stmt)),
                            Err(_) => {
                                // Try to parse as for-of statement
                                parser.restore_position(pos);
                                ForOfStatementParser::new().parse(parser).map(Statement::ForOfStatement)
                            }
                        }
                    }
                }
            },
            Token::While => {
                WhileStatementParser::new().parse(parser).map(Statement::WhileStatement)
            },
            Token::Do => {
                DoWhileStatementParser::new().parse(parser).map(Statement::DoWhileStatement)
            },
            Token::Try => {
                TryStatementParser::new().parse(parser).map(Statement::TryStatement)
            },
            Token::With => {
                WithStatementParser::new().parse(parser).map(Statement::WithStatement)
            },
            Token::Throw => {
                ThrowStatementParser::new().parse(parser).map(Statement::ThrowStatement)
            },
            Token::Return => {
                ReturnStatementParser::new().parse(parser).map(Statement::ReturnStatement)
            },
            Token::Break => {
                BreakStatementParser::new().parse(parser).map(Statement::BreakStatement)
            },
            Token::Continue => {
                ContinueStatementParser::new().parse(parser).map(Statement::ContinueStatement)
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
                let ident = IdentifierParser::new().parse(parser)?;
                
                if parser.consume(&Token::Colon) {
                    // This is a labeled statement
                    let body = Box::new(self.parse(parser)?);
                    Ok(Statement::LabeledStatement(LabeledStatement { label: ident, body }))
                } else {
                    // Not a labeled statement, restore position and parse as expression statement
                    parser.restore_position(pos);
                    ExpressionStatementParser::new().parse(parser).map(Statement::ExpressionStatement)
                }
            },
            // Default to expression statement
            _ => {
                ExpressionStatementParser::new().parse(parser).map(Statement::ExpressionStatement)
            }
        }
    }
}

/// Parser for block statements
pub struct BlockStatementParser;

impl BlockStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<BlockStatement> for BlockStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<BlockStatement> {
        parser.assert_consume(&Token::LeftBrace, "Expected '{' at the start of block statement")?;
        
        let mut body = Vec::new();
        
        while !parser.check(&Token::RightBrace) && !parser.is_at_end() {
            // Parse a statement
            let statement = StatementParser::new().parse(parser)?;
            body.push(statement);
        }
        
        parser.assert_consume(&Token::RightBrace, "Expected '}' at the end of block statement")?;
        
        Ok(BlockStatement { body })
    }
}

/// Parser for expression statements
pub struct ExpressionStatementParser;

impl ExpressionStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ExpressionStatement> for ExpressionStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExpressionStatement> {
        // Check for directive prologue (string literals that might be "use strict")
        if let Token::StringLiteral(_) = parser.peek() {
            let pos = parser.save_position();
            let expr = ExpressionParser::new().parse(parser)?;
            
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
            match ObjectExpressionParser::new().parse(parser) {
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
        let expr = ExpressionParser::new().parse(parser)?;
        
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
pub struct IfStatementParser;

impl IfStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<IfStatement> for IfStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<IfStatement> {
        parser.assert_consume(&Token::If, "Expected 'if'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'if'")?;
        
        let test = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after if condition")?;
        
        let consequent = Box::new(StatementParser::new().parse(parser)?);
        
        let alternate = if parser.consume(&Token::Else) {
            Some(Box::new(StatementParser::new().parse(parser)?))
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
pub struct SwitchStatementParser;

impl SwitchStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<SwitchStatement> for SwitchStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<SwitchStatement> {
        parser.assert_consume(&Token::Switch, "Expected 'switch'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'switch'")?;
        
        let discriminant = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after switch expression")?;
        parser.assert_consume(&Token::LeftBrace, "Expected '{' to start switch body")?;
        
        let cases = parser.with_context(LexicalContext::SwitchBody, |p| {

            let mut result = Vec::new();
        
            while !p.check(&Token::RightBrace) && !p.is_at_end() {
                if p.consume(&Token::Case) {
                    // Case clause
                    let test = Some(Box::new(ExpressionParser::new().parse(p)?));
                    p.assert_consume(&Token::Colon, "Expected ':' after case value")?;
                    
                    let mut consequent = Vec::new();
                    while !p.check(&Token::Case) && 
                          !p.check(&Token::Default) && 
                          !p.check(&Token::RightBrace) && 
                          !p.is_at_end() {
                        consequent.push(StatementParser::new().parse(p)?);
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
                        consequent.push(StatementParser::new().parse(p)?);
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
pub struct WhileStatementParser;

impl WhileStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<WhileStatement> for WhileStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<WhileStatement> {
        parser.assert_consume(&Token::While, "Expected 'while'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after while condition")?;
            
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            StatementParser::new().parse(p)
        })?;

        Ok(WhileStatement {
            test,
            body: Box::new(body),
        })
    }
}

/// Parser for do-while statements
pub struct DoWhileStatementParser;

impl DoWhileStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<DoWhileStatement> for DoWhileStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<DoWhileStatement> {
        parser.assert_consume(&Token::Do, "Expected 'do'")?;
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            StatementParser::new().parse(p)
        })?;

        parser.assert_consume(&Token::While, "Expected 'while' after do block")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after while condition")?;
        parser.assert_consume(&Token::Semicolon, "Expected ';' after while condition")?;

        Ok(DoWhileStatement {
            body: Box::new(body),
            test,
        })
    }
}

/// Parser for for statements
pub struct ForStatementParser;

impl ForStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ForStatement> for ForStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse initialization
        let init = if parser.consume(&Token::Semicolon) {
            None
        } else if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationParser::new().parse(parser)?;
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for initialization")?;
            Some(ForInit::VariableDeclaration(decl))
        } else {
            // Expression
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for initialization")?;
            Some(ForInit::Expression(Box::new(expr)))
        };
        
        // Parse condition
        let test = if parser.consume(&Token::Semicolon) {
            None
        } else {
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::Semicolon, "Expected ';' after for condition")?;
            Some(Box::new(expr))
        };
        
        // Parse update
        let update = if parser.consume(&Token::RightParen) {
            None
        } else {
            let expr = ExpressionParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightParen, "Expected ')' after for clauses")?;
            Some(Box::new(expr))
        };
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            StatementParser::new().parse(p)
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
pub struct ForInStatementParser;

impl ForInStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ForInStatement> for ForInStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForInStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse left side (variable declaration or pattern)
        let left = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationParser::new().parse(parser)?;
            ForInOf::VariableDeclaration(decl)
        } else {
            // Pattern
            let pattern = PatternParser::new().parse(parser)?;
            ForInOf::Pattern(pattern)
        };
        
        // Expect 'in' keyword
        parser.assert_consume(&Token::In, "Expected 'in' in for-in statement")?;
        
        // Parse right side (expression)
        let right = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after for-in clauses")?;
        
        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            StatementParser::new().parse(p)
        })?;
        
        Ok(ForInStatement {
            left,
            right,
            body: Box::new(body),
        })
    }
}

/// Parser for for-of statements
pub struct ForOfStatementParser;

impl ForOfStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ForOfStatement> for ForOfStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ForOfStatement> {
        parser.assert_consume(&Token::For, "Expected 'for'")?;
        
        // Check for 'await' (for await of)
        let await_token = parser.consume(&Token::Await);
        
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse left side (variable declaration or pattern)
        let left = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let decl = VariableDeclarationParser::new().parse(parser)?;
            ForInOf::VariableDeclaration(decl)
        } else {
            // Pattern
            let pattern = PatternParser::new().parse(parser)?;
            ForInOf::Pattern(pattern)
        };
        
        // Expect 'of' keyword
        parser.assert_consume(&Token::Of, "Expected 'of' in for-of statement")?;
        
        // Parse right side (expression)
        let right = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after for-of clauses")?;

        let body = parser.with_context(LexicalContext::LoopBody, |p| {
            StatementParser::new().parse(p)
        })?;
        
        Ok(ForOfStatement {
            left,
            right,
            body: Box::new(body),
            await_token,
        })
    }
}

/// Parser for break statements
pub struct BreakStatementParser;

impl BreakStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<BreakStatement> for BreakStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<BreakStatement> {
        parser.assert_consume(&Token::Break, "Expected 'break'")?;
        
        // Check if we're in a loop or switch
        if !parser.is_in_loop_body() && !parser.is_in_switch() {
            return Err(parser.error_at_current("'break' statement can only be used within a loop or switch statement"));
        }
        
        // Check for label
        let label = if !parser.previous_line_terminator() && matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(BreakStatement { label })
    }
}

/// Parser for continue statements
pub struct ContinueStatementParser;

impl ContinueStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ContinueStatement> for ContinueStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ContinueStatement> {
        parser.assert_consume(&Token::Continue, "Expected 'continue'")?;
        
        // Check if we're in a loop
        if !parser.is_in_loop_body() {
            return Err(parser.error_at_current("'continue' statement can only be used within a loop"));
        }
        
        // Check for label
        let label = if !parser.previous_line_terminator() && matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(ContinueStatement { label })
    }
}

/// Parser for return statements
pub struct ReturnStatementParser;

impl ReturnStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ReturnStatement> for ReturnStatementParser {
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
            Some(Box::new(ExpressionParser::new().parse(parser)?))
        };
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(ReturnStatement { argument })
    }
}

/// Parser for with statements
pub struct WithStatementParser;

impl WithStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<WithStatement> for WithStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<WithStatement> {
        parser.assert_consume(&Token::With, "Expected 'with'")?;
        parser.assert_consume(&Token::LeftParen, "Expected '(' after 'with'")?;
        
        let object = Box::new(ExpressionParser::new().parse(parser)?);
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after with object")?;
        
        let body = Box::new(StatementParser::new().parse(parser)?);
        
        Ok(WithStatement {
            object,
            body,
        })
    }
}

/// Parser for throw statements
pub struct ThrowStatementParser;

impl ThrowStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ThrowStatement> for ThrowStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ThrowStatement> {
        parser.assert_consume(&Token::Throw, "Expected 'throw'")?;
        
        // Line terminator not allowed between throw and expression
        if parser.previous_line_terminator() {
            return Err(parser.error_at_current("Line terminator not allowed after 'throw'"));
        }
        
        let argument = Box::new(ExpressionParser::new().parse(parser)?);
        
        // Consume semicolon if present
        parser.consume(&Token::Semicolon);
        
        Ok(ThrowStatement { argument })
    }
}

/// Parser for try statements
pub struct TryStatementParser;

impl TryStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<TryStatement> for TryStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<TryStatement> {
        parser.assert_consume(&Token::Try, "Expected 'try'")?;
        
        let block = BlockStatementParser::new().parse(parser)?;
        
        // Parse catch clause if present
        let handler = if parser.consume(&Token::Catch) {
            Some(self.parse_catch_clause(parser)?)
        } else {
            None
        };
        
        // Parse finally clause if present
        let finalizer = if parser.consume(&Token::Finally) {
            Some(BlockStatementParser::new().parse(parser)?)
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

impl TryStatementParser {
    fn parse_catch_clause(&self, parser: &mut Parser) -> ParseResult<CatchClause> {
        // The 'catch' keyword has already been consumed
        
        // Parse parameter if present
        let param = if parser.consume(&Token::LeftParen) {
            let pattern = PatternParser::new().parse(parser)?;
            parser.assert_consume(&Token::RightParen, "Expected ')' after catch parameter")?;
            Some(pattern)
        } else {
            None
        };
        
        let body = BlockStatementParser::new().parse(parser)?;
        
        Ok(CatchClause {
            param,
            body,
        })
    }
}

/// Parser for labeled statements
pub struct LabeledStatementParser;

impl LabeledStatementParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<LabeledStatement> for LabeledStatementParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<LabeledStatement> {
        let label = IdentifierParser::new().parse(parser)?;
        
        parser.assert_consume(&Token::Colon, "Expected ':' after label")?;
        
        // Add label to context
        //parser.add_label(label.name.clone());
        
        let body = Box::new(StatementParser::new().parse(parser)?);
        
        // Remove label from context
        //parser.remove_label(&label.name);
        
        Ok(LabeledStatement {
            label,
            body,
        })
    }
}
