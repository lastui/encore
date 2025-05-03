use super::ast::*;
use super::lexer::*;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: &str) -> Self {
        ParseError {
            message: message.to_string(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

impl From<LexerError> for ParseError {
    fn from(error: LexerError) -> Self {
        ParseError::new(&format!("Lexer error: {}", error))
    }
}

// Add a Context struct to track parsing context
#[derive(Clone, Copy)]
struct ParsingContext {
    is_async: bool,
    // TODO is_generator
}

impl ParsingContext {
    fn new() -> Self {
        ParsingContext {
            is_async: false,
        }
    }
    
    fn with_async(self, is_async: bool) -> Self {
        ParsingContext {
            is_async,
            ..self
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
        }
    }
    
    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let context = ParsingContext::new();
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_statement(context)?);
        }
        Ok(Module { statements })
    }
    
    fn parse_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        if self.check(&TokenType::Async) {
            // Save current position in case we need to backtrack
            let current_pos = self.current;
            
            // Consume 'async' token
            self.advance();
            
            // Check if the next token is 'function'
            if self.check(&TokenType::Function) {
                // This is an async function declaration
                // TODO inconsistent
                return self.parse_async_function_declaration(context);
            } else {
                // Not a function declaration, backtrack
                self.current = current_pos;
            }
        }
        
        if self.match_token(&[TokenType::Function]) {
            return self.parse_function_declaration(context);
        }
        
        if self.match_token(&[TokenType::Import]) {
            return self.parse_import_declaration(context);
        }
        
        if self.match_token(&[TokenType::Export]) {
            return self.parse_export_declaration(context);
        }
        
        if self.match_token(&[TokenType::LeftBrace]) {
            return self.parse_block_statement(context);
        }
        
        if self.match_token(&[TokenType::Var, TokenType::Let, TokenType::Const]) {
            // TODO var is special as it is yanked on top
            return self.parse_variable_declaration(context);
        }
        
        if self.match_token(&[TokenType::Function]) {
            return self.parse_function_declaration(context);
        }
        
        if self.match_token(&[TokenType::Return]) {
            return self.parse_return_statement(context);
        }
        
        if self.match_token(&[TokenType::If]) {
            return self.parse_if_statement(context);
        }
        
        if self.match_token(&[TokenType::While]) {
            return self.parse_while_statement(context);
        }
        
        if self.match_token(&[TokenType::For]) {
            return self.parse_for_statement(context);
        }
        
        if self.match_token(&[TokenType::Try]) {
            return self.parse_try_statement(context);
        }
        
        if self.match_token(&[TokenType::Throw]) {
            return self.parse_throw_statement(context);
        }
        
        if self.match_token(&[TokenType::Switch]) {
            return self.parse_switch_statement(context);
        }
        
        if self.match_token(&[TokenType::Break]) {
            return self.parse_break_statement(context);
        }
        
        if self.match_token(&[TokenType::Continue]) {
            return self.parse_continue_statement(context);
        }
        
        if self.match_token(&[TokenType::Semicolon]) {
            return Ok(Statement::EmptyStatement);
        }
        
        // If none of the above, it's an expression statement
        self.parse_expression_statement(context)
    }
    
    fn parse_import_declaration(&mut self, _context: ParsingContext) -> Result<Statement, ParseError> {
        let mut specifiers = Vec::new();
        
        // Check for import without specifiers: import 'module';
        if self.check(&TokenType::StringLiteral(String::new())) {
            let source = self.consume_string_literal("Expected module specifier")?;
            self.consume(&TokenType::Semicolon, "Expected ';' after import")?;
            return Ok(Statement::ImportDeclaration {
                specifiers,
                source,
            });
        }
        
        // Check for default import: import name from 'module';
        if self.check(&TokenType::Identifier(String::new())) {
            let local = self.consume_identifier("Expected import name")?;
            
            specifiers.push(ImportSpecifier::ImportDefaultSpecifier(local));
            
            // Check for additional named imports: import name, { x, y } from 'module';
            if self.match_token(&[TokenType::Comma]) {
                self.parse_named_import_specifiers(&mut specifiers)?;
            }
        } 
        // Check for namespace import: import * as name from 'module';
        else if self.match_token(&[TokenType::Star]) {
            self.consume(&TokenType::As, "Expected 'as' after '*'")?;
            let local = self.consume_identifier("Expected namespace import name")?;
            
            specifiers.push(ImportSpecifier::ImportNamespaceSpecifier(local));
        }
        // Check for named imports: import { x, y } from 'module';
        else if self.check(&TokenType::LeftBrace) {
            self.parse_named_import_specifiers(&mut specifiers)?;
        }
        
        // Parse the module source
        self.consume(&TokenType::From, "Expected 'from' after import specifiers")?;
        let source = self.consume_string_literal("Expected module specifier")?;
        
        self.consume(&TokenType::Semicolon, "Expected ';' after import")?;
        
        Ok(Statement::ImportDeclaration {
            specifiers,
            source,
        })
    }
    
    fn parse_named_import_specifiers(&mut self, specifiers: &mut Vec<ImportSpecifier>) -> Result<(), ParseError> {
        self.consume(&TokenType::LeftBrace, "Expected '{' for named imports")?;
        
        // Parse comma-separated list of import specifiers
        while !self.check(&TokenType::RightBrace) {
            if !specifiers.is_empty() && self.check(&TokenType::Comma) {
                self.advance();
                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
            
            let imported = self.consume_identifier("Expected imported name")?;
            let local;
            
            // Handle aliased imports: import { x as y } from 'module';
            if self.match_token(&[TokenType::As]) {
                local = self.consume_identifier("Expected local name after 'as'")?;
            } else {
                local = imported.clone();
            }
            
            specifiers.push(ImportSpecifier::ImportSpecifier {
                imported,
                local,
            });
            
            if !self.check(&TokenType::RightBrace) {
                self.consume(&TokenType::Comma, "Expected ',' between import specifiers")?;
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after named imports")?;
        
        Ok(())
    }
    
    fn parse_export_declaration(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        // Check for re-export: export * from 'module';
        if self.match_token(&[TokenType::Star]) {
            self.consume(&TokenType::From, "Expected 'from' after export *")?;
            let source = self.consume_string_literal("Expected module specifier")?;
            self.consume(&TokenType::Semicolon, "Expected ';' after export")?;
            
            return Ok(Statement::ExportAllDeclaration { source });
        }
        
        // Check for default export: export default expression;
        if self.match_token(&[TokenType::Default]) {
            let declaration = self.parse_expression(context)?;
            self.consume(&TokenType::Semicolon, "Expected ';' after export")?;
            
            return Ok(Statement::ExportDefaultDeclaration {
                declaration: Box::new(declaration),
            });
        }
        
        // Check for named exports: export { x, y };
        if self.check(&TokenType::LeftBrace) {
            let mut specifiers = Vec::new();
            
            self.consume(&TokenType::LeftBrace, "Expected '{' for named exports")?;
            
            // Parse comma-separated list of export specifiers
            while !self.check(&TokenType::RightBrace) {
                if !specifiers.is_empty() {
                    self.consume(&TokenType::Comma, "Expected ',' between export specifiers")?;
                    if self.check(&TokenType::RightBrace) {
                        break;
                    }
                }
                
                let local = self.consume_identifier("Expected exported name")?;
                let exported;
                
                // Handle aliased exports: export { x as y };
                if self.match_token(&[TokenType::As]) {
                    exported = self.consume_identifier("Expected exported name after 'as'")?;
                } else {
                    exported = local.clone();
                }
                
                specifiers.push(ExportSpecifier::ExportSpecifier {
                    local,
                    exported,
                });
            }
            
            self.consume(&TokenType::RightBrace, "Expected '}' after named exports")?;
            
            // Check for re-exports: export { x, y } from 'module';
            let source = if self.match_token(&[TokenType::From]) {
                Some(self.consume_string_literal("Expected module specifier")?)
            } else {
                None
            };
            
            self.consume(&TokenType::Semicolon, "Expected ';' after export")?;
            
            return Ok(Statement::ExportNamedDeclaration {
                declaration: None,
                specifiers,
                source,
            });
        }
        
        // Check for declaration exports: export function x() {}, export const x = 1;
        let declaration = self.parse_declaration(context)?;
        
        Ok(Statement::ExportNamedDeclaration {
            declaration: Some(Box::new(declaration)),
            specifiers: Vec::new(),
            source: None,
        })
    }
    
    fn parse_declaration(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        if self.match_token(&[TokenType::Var, TokenType::Let, TokenType::Const]) {
            return self.parse_variable_declaration(context);
        }
        
        if self.match_token(&[TokenType::Function]) {
            return self.parse_function_declaration(context);
        }
        
        // If it's not a declaration, it's an error
        Err(ParseError::new("Expected variable or function declaration"))
    }
    
    fn parse_block_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        let mut statements = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement(context)?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after block")?;
        
        Ok(Statement::BlockStatement { body: statements })
    }
    
    fn parse_variable_declaration(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        let kind = match self.previous().token_type {
            TokenType::Var => VariableKind::Var,
            TokenType::Let => VariableKind::Let,
            TokenType::Const => VariableKind::Const,
            _ => unreachable!(),
        };
        
        let mut declarations = Vec::new();
        
        // Parse first declarator
        declarations.push(self.parse_variable_declarator(context)?);
        
        // Parse additional declarators
        while self.match_token(&[TokenType::Comma]) {
            declarations.push(self.parse_variable_declarator(context)?);
        }
        
        self.consume(&TokenType::Semicolon, "Expected ';' after variable declaration")?;
        
        Ok(Statement::VariableDeclaration { declarations, kind })
    }
    
    fn parse_variable_declarator(&mut self, context: ParsingContext) -> Result<VariableDeclarator, ParseError> {
        let id = self.consume_identifier("Expected variable name")?;
        
        let init = if self.match_token(&[TokenType::Equal]) {
            Some(self.parse_expression(context)?)
        } else {
            None
        };
        
        Ok(VariableDeclarator { id, init })
    }
    
    fn parse_function_declaration(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        // Consume the 'function' keyword
        self.consume(&TokenType::Function, "Expected 'function' keyword")?;
        
        // Get the function name
        let id = self.consume_identifier("Expected function name")?;
        
        // Parse parameters
        self.consume(&TokenType::LeftParen, "Expected '(' after function name")?;
        let params = self.parse_function_parameters()?;
        self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
        
        // Parse function body
        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        
        let mut body = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            body.push(self.parse_statement(context)?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
        // Return regular function declaration
        Ok(Statement::FunctionDeclaration { id, params, body })
    }

    fn parse_arrow_function_body(&mut self, params: Vec<FunctionParameter>, context: ParsingContext) -> Result<Expression, ParseError> {
        // Arrow function body can be an expression or a block
        if self.match_token(&[TokenType::LeftBrace]) {
            // Block body
            let mut body = Vec::new();
            
            while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                body.push(self.parse_statement(context)?);
            }
            
            self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
            
            Ok(Expression::ArrowFunctionExpression {
                params,
                body: ArrowFunctionBody::BlockStatement(body),
                is_async: context.is_async,
            })
        } else {
            // Expression body
            let expr = self.parse_expression(context)?;
            
            Ok(Expression::ArrowFunctionExpression {
                params,
                body: ArrowFunctionBody::Expression(Box::new(expr)),
                is_async: context.is_async,
            })
        }
    }
    
    fn parse_async_function_declaration(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        // We've already consumed the 'async' keyword
        
        // Consume the 'function' keyword
        self.consume(&TokenType::Function, "Expected 'function' keyword after 'async'")?;
        
        // Get the function name
        let id = self.consume_identifier("Expected function name")?;
        
        // Parse parameters
        self.consume(&TokenType::LeftParen, "Expected '(' after function name")?;
        let params = self.parse_function_parameters()?;
        self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
        
        // Parse function body
        self.consume(&TokenType::LeftBrace, "Expected '{' before function body")?;
        
        // Use a new context with is_async set to true
        let async_context = context.with_async(true);
        let mut body = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            body.push(self.parse_statement(async_context)?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after function body")?;
        
        Ok(Statement::AsyncFunctionDeclaration { id, params, body })
    }
    
    fn parse_function_parameters(&mut self) -> Result<Vec<FunctionParameter>, ParseError> {
        let mut params = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            // Parse first parameter
            params.push(self.parse_function_parameter()?);
            
            // Parse additional parameters
            while self.match_token(&[TokenType::Comma]) {
                if self.check(&TokenType::RightParen) {
                    break;
                }
                params.push(self.parse_function_parameter()?);
            }
        }
        
        Ok(params)
    }
    
    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
        // Check for rest parameter
        let is_rest = self.match_token(&[TokenType::Ellipsis]);
        
        // Get parameter name
        let name = self.consume_identifier("Expected parameter name")?;
        
        // Check for default value
        let default_value = if self.match_token(&[TokenType::Equal]) {
            let context = ParsingContext::new();
            Some(self.parse_expression(context)?)
        } else {
            None
        };
        
        Ok(FunctionParameter {
            name,
            is_rest,
            default_value,
        })
    }
    
    fn parse_return_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        let argument = if !self.check(&TokenType::Semicolon) {
            Some(self.parse_expression(context)?)
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after return statement")?;
        
        Ok(Statement::ReturnStatement { argument })
    }
    
    fn parse_if_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'if'")?;
        let test = self.parse_expression(context)?;
        self.consume(&TokenType::RightParen, "Expected ')' after if condition")?;
        
        let consequent = self.parse_statement(context)?;
        
        let alternate = if self.match_token(&[TokenType::Else]) {
            Some(Box::new(self.parse_statement(context)?))
        } else {
            None
        };
        
        Ok(Statement::IfStatement {
            test,
            consequent: Box::new(consequent),
            alternate,
        })
    }
    
    fn parse_while_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        let test = self.parse_expression(context)?;
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        
        let body = self.parse_statement(context)?;
        
        Ok(Statement::WhileStatement {
            test,
            body: Box::new(body),
        })
    }
    
    fn parse_for_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse initialization
        let init = if self.match_token(&[TokenType::Semicolon]) {
            None
        } else if self.match_token(&[TokenType::Var, TokenType::Let, TokenType::Const]) {

            // TODO var is special as it is hoisted on top
            let kind = match self.previous().token_type {
                TokenType::Var => VariableKind::Var,
                TokenType::Let => VariableKind::Let,
                TokenType::Const => VariableKind::Const,
                _ => unreachable!(),
            };
            
            let mut declarations = Vec::new();
            
            // Parse first declarator
            declarations.push(self.parse_variable_declarator(context)?);
            
            // Parse additional declarators
            while self.match_token(&[TokenType::Comma]) {
                declarations.push(self.parse_variable_declarator(context)?);
            }
            
            self.consume(&TokenType::Semicolon, "Expected ';' after for loop initialization")?;
            
            Some(ForInit::VariableDeclaration { declarations, kind })
        } else {
            let expr = self.parse_expression(context)?;
            self.consume(&TokenType::Semicolon, "Expected ';' after for loop initialization")?;
            Some(ForInit::Expression(expr))
        };
        
        // Parse condition
        let test = if !self.check(&TokenType::Semicolon) {
            Some(self.parse_expression(context)?)
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after for loop condition")?;
        
        // Parse update
        let update = if !self.check(&TokenType::RightParen) {
            Some(self.parse_expression(context)?)
        } else {
            None
        };
        
        self.consume(&TokenType::RightParen, "Expected ')' after for loop clauses")?;
        
        // Parse body
        let body = self.parse_statement(context)?;
        
        Ok(Statement::ForStatement {
            init,
            test,
            update,
            body: Box::new(body),
        })
    }
    
    fn parse_try_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        // Parse try block
        self.consume(&TokenType::LeftBrace, "Expected '{' after 'try'")?;
        let try_block = self.parse_block_statement(context)?;
        
        // Parse catch clause
        let catch_clause = if self.match_token(&[TokenType::Catch]) {
            let param = if self.match_token(&[TokenType::LeftParen]) {
                let param = self.consume_identifier("Expected catch parameter name")?;
                self.consume(&TokenType::RightParen, "Expected ')' after catch parameter")?;
                Some(param)
            } else {
                None
            };
            
            self.consume(&TokenType::LeftBrace, "Expected '{' after catch")?;
            let body = self.parse_block_statement(context)?;
            
            Some(CatchClause { param, body: Box::new(body) })
        } else {
            None
        };
        
        // Parse finally block
        let finally_block = if self.match_token(&[TokenType::Finally]) {
            self.consume(&TokenType::LeftBrace, "Expected '{' after 'finally'")?;
            Some(Box::new(self.parse_block_statement(context)?))
        } else {
            None
        };
        
        // Ensure at least one of catch or finally is present
        if catch_clause.is_none() && finally_block.is_none() {
            return Err(ParseError::new("Try statement must have either catch or finally clause"));
        }
        
        Ok(Statement::TryStatement {
            block: Box::new(try_block),
            handler: catch_clause,
            finalizer: finally_block,
        })
    }
    
    fn parse_throw_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        let argument = self.parse_expression(context)?;
        self.consume(&TokenType::Semicolon, "Expected ';' after throw statement")?;
        Ok(Statement::ThrowStatement { argument })
    }
    
    fn parse_switch_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'switch'")?;
        let discriminant = self.parse_expression(context)?;
        self.consume(&TokenType::RightParen, "Expected ')' after switch discriminant")?;
        
        self.consume(&TokenType::LeftBrace, "Expected '{' after switch discriminant")?;
        
        let mut cases = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            if self.match_token(&[TokenType::Case]) {
                let test = self.parse_expression(context)?;
                self.consume(&TokenType::Colon, "Expected ':' after case value")?;
                
                let mut consequent = Vec::new();
                
                while !self.check(&TokenType::Case) && !self.check(&TokenType::Default) && !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                    consequent.push(self.parse_statement(context)?);
                }
                
                cases.push(SwitchCase {
                    test: Some(test),
                    consequent,
                });
            } else if self.match_token(&[TokenType::Default]) {
                self.consume(&TokenType::Colon, "Expected ':' after default case")?;
                
                let mut consequent = Vec::new();
                
                while !self.check(&TokenType::Case) && !self.check(&TokenType::Default) && !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                    consequent.push(self.parse_statement(context)?);
                }
                
                cases.push(SwitchCase {
                    test: None,
                    consequent,
                });
            } else {
                return Err(ParseError::new("Expected 'case' or 'default' in switch statement"));
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after switch cases")?;
        
        Ok(Statement::SwitchStatement {
            discriminant,
            cases,
        })
    }
    
    fn parse_break_statement(&mut self, _context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::Semicolon, "Expected ';' after break statement")?;
        
        Ok(Statement::BreakStatement)
    }
    
    fn parse_continue_statement(&mut self, _context: ParsingContext) -> Result<Statement, ParseError> {
        self.consume(&TokenType::Semicolon, "Expected ';' after continue statement")?;
        
        Ok(Statement::ContinueStatement)
    }
    
    fn parse_expression_statement(&mut self, context: ParsingContext) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(context)?;
        self.consume(&TokenType::Semicolon, "Expected ';' after expression")?;
        
        Ok(Statement::ExpressionStatement { expression: expr })
    }
    
    fn parse_expression(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        self.parse_assignment(context)
    }
    
    fn parse_assignment(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let expr = self.parse_conditional(context)?;
        
        if self.match_token(&[TokenType::Equal, TokenType::PlusEqual, TokenType::MinusEqual, TokenType::StarEqual, TokenType::SlashEqual]) {
            let operator = match self.previous().token_type {
                TokenType::Equal => AssignmentOperator::Assign,
                TokenType::PlusEqual => AssignmentOperator::AddAssign,
                TokenType::MinusEqual => AssignmentOperator::SubtractAssign,
                TokenType::StarEqual => AssignmentOperator::MultiplyAssign,
                TokenType::SlashEqual => AssignmentOperator::DivideAssign,
                _ => unreachable!(),
            };
            
            let right = self.parse_assignment(context)?;
            
            // Check if left side is a valid assignment target
            match expr {
                Expression::Identifier(_) => {
                    return Ok(Expression::AssignmentExpression {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    });
                }
                Expression::MemberExpression { .. } => {
                    return Ok(Expression::AssignmentExpression {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    });
                }
                _ => return Err(ParseError::new("Invalid assignment target")),
            }
        }
        
        Ok(expr)
    }
    
    fn parse_conditional(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let expr = self.parse_logical_or(context)?;
        
        // TODO greedy double look ahead
        if self.match_token(&[TokenType::Question]) {
            let consequent = self.parse_expression(context)?;
            self.consume(&TokenType::Colon, "Expected ':' in conditional expression")?;
            let alternate = self.parse_conditional(context)?;
            
            return Ok(Expression::ConditionalExpression {
                test: Box::new(expr),
                consequent: Box::new(consequent),
                alternate: Box::new(alternate),
            });
        }
        
        Ok(expr)
    }

    fn parse_logical_or(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_and(context)?;
            
        // TODO greedy double look ahead
        while self.match_token(&[TokenType::Or]) {
            let right = self.parse_logical_and(context)?;
            expr = Expression::LogicalExpression {
                left: Box::new(expr),
                operator: LogicalOperator::Or,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_logical_and(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_equality(context)?;
            
        // TODO greedy double look ahead
        while self.match_token(&[TokenType::And]) {
            let right = self.parse_equality(context)?;
            expr = Expression::LogicalExpression {
                left: Box::new(expr),
                operator: LogicalOperator::And,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_equality(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_comparison(context)?;
            
        // TODO greedy double look ahead
        while self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = match self.previous().token_type {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            
            let right = self.parse_comparison(context)?;
            expr = Expression::BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_addition(context)?;
        
        // TODO greedy double look ahead
        while self.match_token(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = match self.previous().token_type {
                TokenType::Greater => BinaryOperator::GreaterThan,
                TokenType::GreaterEqual => BinaryOperator::GreaterThanEqual,
                TokenType::Less => BinaryOperator::LessThan,
                TokenType::LessEqual => BinaryOperator::LessThanEqual,
                _ => unreachable!(),
            };
            
            let right = self.parse_addition(context)?;
            expr = Expression::BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_addition(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_multiplication(context)?;
        
        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let operator = match self.previous().token_type {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            
            let right = self.parse_multiplication(context)?;
            expr = Expression::BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_multiplication(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary(context)?;
        
        while self.match_token(&[TokenType::Star, TokenType::Slash]) {
            let operator = match self.previous().token_type {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            
            let right = self.parse_unary(context)?;
            expr = Expression::BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus, TokenType::Plus, TokenType::TypeOf]) {
            let operator = match self.previous().token_type {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Negate,
                TokenType::Plus => UnaryOperator::Plus,
                TokenType::TypeOf => UnaryOperator::TypeOf,
                _ => unreachable!(),
            };
            
            let right = self.parse_unary(context)?;
            return Ok(Expression::UnaryExpression {
                operator,
                argument: Box::new(right),
            });
        }
        
        // Check for async arrow function
        if self.check(&TokenType::Async) {
            // Save current position in case we need to backtrack
            let current_pos = self.current;
            
            // Consume 'async' token
            self.advance();
            
            // Check if this might be an async arrow function
            if self.check(&TokenType::LeftParen) || self.check(&TokenType::Identifier(String::new())) {
                // Try to parse as an async arrow function
                if let Ok(expr) = self.try_parse_arrow_function(context.with_async(true)) {
                    return Ok(expr);
                }
                
                // If parsing as arrow function failed, backtrack
                self.current = current_pos;
            } else {
                // Not an arrow function, backtrack
                self.current = current_pos;
            }
        }
        
        // Try to parse as an arrow function
        if self.check(&TokenType::LeftParen) || self.check(&TokenType::Identifier(String::new())) {
            if let Ok(expr) = self.try_parse_arrow_function(context) {
                return Ok(expr);
            }
        }
        
        self.parse_call(context)
    }
    
    fn try_parse_arrow_function(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        // Save current position in case we need to backtrack
        let current_pos = self.current;
        
        let params = if self.check(&TokenType::Identifier(String::new())) {
            // Single parameter without parentheses
            let name = self.consume_identifier("Expected parameter name")?;
            vec![FunctionParameter {
                name,
                is_rest: false,
                default_value: None,
            }]
        } else if self.match_token(&[TokenType::LeftParen]) {
            // Parameter list with parentheses
            let params = self.parse_function_parameters()?;
            self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
            params
        } else {
            // Not an arrow function
            self.current = current_pos;
            return Err(ParseError::new("Not an arrow function"));
        };
        
        // Check for arrow token
        if !self.match_token(&[TokenType::Arrow]) {
            // Not an arrow function
            self.current = current_pos;
            return Err(ParseError::new("Not an arrow function"));
        }
        
        // Parse arrow function body
        self.parse_arrow_function_body(params, context)
    }
    
    fn parse_call(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary(context)?;
        
        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr, context)?;
            } else if self.match_token(&[TokenType::Dot]) {
                let property = self.consume_identifier("Expected property name after '.'")?;
                expr = Expression::MemberExpression {
                    object: Box::new(expr),
                    property: Box::new(Expression::Identifier(property)),
                    computed: false,
                };
            } else if self.match_token(&[TokenType::LeftBracket]) {
                let property = self.parse_expression(context)?;
                self.consume(&TokenType::RightBracket, "Expected ']' after property name")?;
                expr = Expression::MemberExpression {
                    object: Box::new(expr),
                    property: Box::new(property),
                    computed: true,
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn finish_call(&mut self, callee: Expression, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut arguments = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            // Parse first argument
            arguments.push(self.parse_expression(context)?);
            
            // Parse additional arguments
            while self.match_token(&[TokenType::Comma]) {
                if self.check(&TokenType::RightParen) {
                    break;
                }
                arguments.push(self.parse_expression(context)?);
            }
        }
        
        self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
        
        Ok(Expression::CallExpression {
            callee: Box::new(callee),
            arguments,
        })
    }
    
    fn parse_primary(&mut self, _context: ParsingContext) -> Result<Expression, ParseError> {
        if self.match_token(&[TokenType::This]) {
            return Ok(Expression::ThisExpression);
        }
        
        if self.match_token(&[TokenType::Identifier(String::new())]) {
            if let TokenType::Identifier(name) = &self.previous().token_type {
                return Ok(Expression::Identifier(name.clone()));
            }
            unreachable!();
        }
        
        if self.match_token(&[TokenType::NumberLiteral(0.0)]) {
            if let TokenType::NumberLiteral(value) = self.previous().token_type {
                return Ok(Expression::NumberLiteral(value));
            }
            unreachable!();
        }
        
        if self.match_token(&[TokenType::StringLiteral(String::new())]) {
            if let TokenType::StringLiteral(value) = &self.previous().token_type {
                return Ok(Expression::StringLiteral(value.clone()));
            }
            unreachable!();
        }
        
        if self.match_token(&[TokenType::True]) {
            return Ok(Expression::BooleanLiteral(true));
        }
        
        if self.match_token(&[TokenType::False]) {
            return Ok(Expression::BooleanLiteral(false));
        }
        
        if self.match_token(&[TokenType::Null]) {
            return Ok(Expression::NullLiteral);
        }
        
        if self.match_token(&[TokenType::Undefined]) {
            return Ok(Expression::UndefinedLiteral);
        }
        
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.parse_expression(ParsingContext::new())?;
            self.consume(&TokenType::RightParen, "Expected ')' after expression")?;
            return Ok(expr);
        }
        
        if self.match_token(&[TokenType::LeftBracket]) {
            return self.parse_array_literal(ParsingContext::new());
        }
        
        if self.match_token(&[TokenType::LeftBrace]) {
            return self.parse_object_literal(ParsingContext::new());
        }
        
        if self.match_token(&[TokenType::New]) {
            return self.parse_new_expression(ParsingContext::new());
        }
        
        Err(ParseError::new(&format!("Unexpected token: {:?}", self.peek())))
    }
    
    fn parse_array_literal(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut elements = Vec::new();
        
        if !self.check(&TokenType::RightBracket) {
            // Parse first element
            if self.check(&TokenType::Comma) {
                elements.push(None);
            } else {
                elements.push(Some(self.parse_expression(context)?));
            }
            
            // Parse additional elements
            while self.match_token(&[TokenType::Comma]) {
                if self.check(&TokenType::RightBracket) {
                    break;
                }
                
                if self.check(&TokenType::Comma) {
                    elements.push(None);
                } else {
                    elements.push(Some(self.parse_expression(context)?));
                }
            }
        }
        
        self.consume(&TokenType::RightBracket, "Expected ']' after array elements")?;
        
        Ok(Expression::ArrayLiteral { elements })
    }
    
    fn parse_object_literal(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let mut properties = Vec::new();
        
        if !self.check(&TokenType::RightBrace) {
            // Parse first property
            properties.push(self.parse_object_property(context)?);
            
            // Parse additional properties
            while self.match_token(&[TokenType::Comma]) {
                if self.check(&TokenType::RightBrace) {
                    break;
                }
                properties.push(self.parse_object_property(context)?);
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after object properties")?;
        
        Ok(Expression::ObjectLiteral { properties })
    }
    
    fn parse_object_property(&mut self, context: ParsingContext) -> Result<ObjectProperty, ParseError> {
        // Check for shorthand property
        if self.check(&TokenType::Identifier(String::new())) {
            let key = self.consume_identifier("Expected property name")?;
            
            // Check if this is a method
            if self.check(&TokenType::LeftParen) {
                // Method definition
                self.consume(&TokenType::LeftParen, "Expected '(' after method name")?;
                let params = self.parse_function_parameters()?;
                self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
                
                self.consume(&TokenType::LeftBrace, "Expected '{' before method body")?;
                
                let mut body = Vec::new();
                
                while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                    body.push(self.parse_statement(context)?);
                }
                
                self.consume(&TokenType::RightBrace, "Expected '}' after method body")?;
                
                return Ok(ObjectProperty::Method {
                    key,
                    params,
                    body,
                });
            }
            
            // Check if this is a property with value
            if self.match_token(&[TokenType::Colon]) {
                let value = self.parse_expression(context)?;
                return Ok(ObjectProperty::Property {
                    key,
                    value,
                });
            }
            // Shorthand property (key only)
            return Ok(ObjectProperty::Shorthand {
                key: key.clone(),
                value: Expression::Identifier(key),
            });
        }
        
        // Computed property name
        if self.match_token(&[TokenType::LeftBracket]) {
            let key = self.parse_expression(context)?;
            self.consume(&TokenType::RightBracket, "Expected ']' after computed property name")?;
            
            self.consume(&TokenType::Colon, "Expected ':' after property name")?;
            let value = self.parse_expression(context)?;
            
            return Ok(ObjectProperty::ComputedProperty {
                key,
                value,
            });
        }
        
        // String or number literal property name
        let key = if self.match_token(&[TokenType::StringLiteral(String::new())]) {
            if let TokenType::StringLiteral(value) = &self.previous().token_type {
                value.clone()
            } else {
                unreachable!()
            }
        } else if self.match_token(&[TokenType::NumberLiteral(0.0)]) {
            if let TokenType::NumberLiteral(value) = self.previous().token_type {
                value.to_string()
            } else {
                unreachable!()
            }
        } else {
            return Err(ParseError::new("Expected property name"));
        };
        
        self.consume(&TokenType::Colon, "Expected ':' after property name")?;
        let value = self.parse_expression(context)?;
        
        Ok(ObjectProperty::Property {
            key,
            value,
        })
    }
    
    fn parse_new_expression(&mut self, context: ParsingContext) -> Result<Expression, ParseError> {
        let callee = self.parse_primary(context)?;
        
        self.consume(&TokenType::LeftParen, "Expected '(' after new expression")?;
        
        let mut arguments = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            // Parse first argument
            arguments.push(self.parse_expression(context)?);
            
            // Parse additional arguments
            while self.match_token(&[TokenType::Comma]) {
                if self.check(&TokenType::RightParen) {
                    break;
                }
                arguments.push(self.parse_expression(context)?);
            }
        }
        
        self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
        
        Ok(Expression::NewExpression {
            callee: Box::new(callee),
            arguments,
        })
    }
    
    // Helper methods
    
    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<(), ParseError> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(&format!("{}: expected {:?}, got {:?}", message, token_type, self.peek().token_type)))
        }
    }
    
    fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        if let TokenType::Identifier(_) = &self.peek().token_type {
            if let TokenType::Identifier(name) = &self.advance().token_type {
                Ok(name.clone())
            } else {
                unreachable!()
            }
        } else {
            Err(ParseError::new(message))
        }
    }
    
    fn consume_string_literal(&mut self, message: &str) -> Result<String, ParseError> {
        if let TokenType::StringLiteral(_) = &self.peek().token_type {
            if let TokenType::StringLiteral(value) = &self.advance().token_type {
                Ok(value.clone())
            } else {
                unreachable!()
            }
        } else {
            Err(ParseError::new(message))
        }
    }
    
    fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }
    
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        
        match (token_type, &self.peek().token_type) {
            (TokenType::Identifier(_), TokenType::Identifier(_)) => true,
            (TokenType::StringLiteral(_), TokenType::StringLiteral(_)) => true,
            (TokenType::NumberLiteral(_), TokenType::NumberLiteral(_)) => true,
            (a, b) => a == b,
        }
    }
    
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    
    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }
    
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
