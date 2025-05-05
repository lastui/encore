use super::prelude::*;


use crate::ast::*;
use crate::lexer::{Token, TokenType, LexicalContext};
use super::error::{ParserError, ParseResult};
use super::core::Parser;

impl Parser {

   // Variable declarations
    pub fn parse_variable_declaration(&mut self) -> ParseResult<VariableDeclaration> {
        let token = self.advance().cloned().unwrap_or_else(|| Token::new(TokenType::EOF, 0, 0, 0));
        
        let kind = match token.token_type {
            TokenType::Var => VariableKind::Var,
            TokenType::Let => VariableKind::Let,
            TokenType::Const => VariableKind::Const,
            _ => unreachable!(),
        };
        
        // Parse first declarator (required)
        let mut declarations = vec![self.parse_variable_declarator()?];
        
        // Parse additional declarators separated by commas
        while self.match_token(&TokenType::Comma) {
            declarations.push(self.parse_variable_declarator()?);
        }

        // Consume semicolon unless we're in a for-in/of loop context
        let current_context = self.current_context();
        let is_in_loop_parameters = matches!(current_context, LexicalContext::LoopParameters);
        
        if !is_in_loop_parameters {
            self.consume(&TokenType::Semicolon, "Expected ';' after variable declaration")?;
        }
        
        Ok(VariableDeclaration { declarations, kind })
    }


    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek_token_type() {
            // Empty statement (just a semicolon)
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(Statement::Empty)
            },

            // Block statement { ... }
            Some(TokenType::LeftBrace) => self.parse_block(),
            
            // Declaration statements
            Some(TokenType::Var) | Some(TokenType::Let) | Some(TokenType::Const) => self.parse_variable_statement(),
            Some(TokenType::Function) =>  self.parse_function_statement(),
            Some(TokenType::Class) =>  self.parse_class_statement(),
            
            // Control flow statements
            Some(TokenType::If) => self.parse_if(),
            Some(TokenType::Switch) => self.parse_switch(),
            Some(TokenType::For) => self.parse_for(),
            Some(TokenType::While) => self.parse_while(),
            Some(TokenType::Do) => self.parse_do_while(),
            
            // Exception handling
            Some(TokenType::Try) => self.parse_try(),
            Some(TokenType::Throw) => self.parse_throw(),
            
            // Function control
            Some(TokenType::Return) => self.parse_return(),
            Some(TokenType::Break) => self.parse_break(),
            Some(TokenType::Continue) => self.parse_continue(),
            
            // Module statements
            Some(TokenType::Import) => self.parse_import_statement(),
            Some(TokenType::Export) => self.parse_export_statement(),
            
            // Other statements
            Some(TokenType::With) => self.parse_with(),
            Some(TokenType::Debugger) => self.parse_debugger(),
            
            // Labeled statement
            Some(TokenType::Identifier(_)) if self.is_label() => self.parse_labeled(),
            
            // Default: expression statement
            _ => self.parse_expression_statement(),
        }
    }

    /// Parse a block statement: { statements... }
    fn parse_block(&mut self) -> ParseResult<Statement> {
        self.consume(&TokenType::LeftBrace, "Expected '{'")?;
        
        let mut statements = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}'")?;
        
        Ok(Statement::Block(statements))
    }

    /// Parse variable declarations as a statement
    fn parse_variable_statement(&mut self) -> ParseResult<Statement> {
        let declaration = self.parse_variable_declaration()?;
        Ok(Statement::Declaration(Declaration::Variable(declaration)))
    }

    /// Parse function declaration as a statement
    fn parse_function_statement(&mut self) -> ParseResult<Statement> {
        let declaration = self.parse_function_declaration()?;
        Ok(Statement::Declaration(Declaration::Function(declaration)))
    }

    /// Parse class declaration as a statement
    fn parse_class_statement(&mut self) -> ParseResult<Statement> {
        let declaration = self.parse_class_declaration()?;
        Ok(Statement::Declaration(Declaration::Class(declaration)))
    }

    /// Parse if statement: if (condition) consequent else alternate
    fn parse_if(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'if'
        self.consume(&TokenType::LeftParen, "Expected '(' after 'if'")?;
        
        let test = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after if condition")?;
        
        let consequent = Box::new(self.parse_statement()?);
        let alternate = self.match_token(&TokenType::Else)
            .then(|| self.parse_statement().map(Box::new))
            .transpose()?;
        
        Ok(Statement::If { test, consequent, alternate })
    }

    /// Parse a single case in a switch statement
    fn parse_switch_case(&mut self) -> ParseResult<SwitchCase> {
        let test = if self.match_token(&TokenType::Case) {
            // After 'case', we expect an expression
            Some(self.parse_expression()?)
        } else if self.match_token(&TokenType::Default) {
            None
        } else {
            println!("Current token {:#?}", self.peek_token());
            return Err(parser_error_at_current!(self, "Expected 'case' or 'default'"));
        };
        
        self.consume(&TokenType::Colon, "Expected ':' after case value")?;
        
        let mut consequent = Vec::new();
        
        // Parse statements until next case, default, or end of switch
        while !self.check(&TokenType::Case) && 
              !self.check(&TokenType::Default) && 
              !self.check(&TokenType::RightBrace) && 
              !self.is_at_end() {
            consequent.push(self.parse_statement()?);
        }
        
        Ok(SwitchCase { test, consequent })
    }

    /// Parse try statement: try block [catch] [finally]
    fn parse_try(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'try'
        
        let block = Box::new(self.parse_block()?);
        
        // Parse optional catch clause
        let handler = self.match_token(&TokenType::Catch)
            .then(|| self.parse_catch_clause())
            .transpose()?;
        
        // Parse optional finally clause
        let finalizer = self.match_token(&TokenType::Finally)
            .then(|| self.parse_block().map(Box::new))
            .transpose()?;
        
        // Either catch or finally must be present
        if handler.is_none() && finalizer.is_none() {
            return Err(parser_error_at_current!(self, "Expected 'catch' or 'finally' after try block"));
        }
        
        Ok(Statement::Try { block, handler, finalizer })
    }

    /// Parse catch clause: catch ([param]) block
    fn parse_catch_clause(&mut self) -> ParseResult<CatchClause> {
        // Optional catch parameter
        let param = self.match_token(&TokenType::LeftParen)
            .then(|| {
                // Attempt to parse the parameter identifier
                if let Some(TokenType::Identifier(name)) = self.peek_token_type().cloned() {
                    self.advance(); // Consume the identifier
                    self.consume(&TokenType::RightParen, "Expected ')' after catch parameter")?;
                    Ok(Expression::Identifier(name.into_boxed_str()))
                } else {
                    // If not an identifier, it's an error
                    Err(parser_error_at_current!(self, "Expected identifier for catch parameter"))
                }
            })
            .transpose()?;
        
        let body = Box::new(self.parse_block()?);

        Ok(CatchClause { param, body })

    }

    /// Parse throw statement: throw expression;
    fn parse_throw(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'throw'
        
        // No line terminator allowed between throw and expression
        if self.previous_line_terminator() {
            return Err(parser_error_at_current!(self, "Illegal newline after throw"));
        }
        
        let expr = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after throw statement")?;
        
        Ok(Statement::Throw(expr))
    }

    /// Parse with statement: with (object) statement
    fn parse_with(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'with'
        
        // Check if in strict mode
        if self.state.in_strict_mode {
            return Err(parser_error_at_current!(self, "'with' statements are not allowed in strict mode"));
        }
        
        self.consume(&TokenType::LeftParen, "Expected '(' after 'with'")?;
        
        let object = self.parse_expression()?;
        
        self.consume(&TokenType::RightParen, "Expected ')' after with expression")?;
        
        let body = Box::new(self.parse_statement()?);
        
        Ok(Statement::With { object, body })
    }

    /// Parse debugger statement: debugger;
    fn parse_debugger(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'debugger'
        
        self.consume(&TokenType::Semicolon, "Expected ';' after debugger statement")?;
        
        Ok(Statement::Debugger)
    }

    /// Parse labeled statement: identifier: statement
    fn parse_labeled(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap();
        let label = self.expect_identifier("Expected label name")?;

        
        self.consume(&TokenType::Colon, "Expected ':' after label")?;
        
        // Add label to the set of active labels
        let label_exists = !self.state.labels.insert(label.clone());
        if label_exists {
            return Err(parser_error_at_current!(self, "Label '{}' has already been declared", label));
        }
        
        // Parse the labeled statement
        let body = Box::new(self.parse_statement()?);
        
        // Remove the label from the set
        self.state.labels.remove(&label);
        
        Ok(Statement::Labeled { label, body })
    }

    /// Check if the current token is a label
    fn is_label(&self) -> bool {
        // Check if the current token is an identifier and the next token is a colon
        if let Some(TokenType::Identifier(_)) = self.peek_token_type() {
            if let Some(next_token) = self.tokens.get(self.current + 1) {
                return matches!(next_token.token_type, TokenType::Colon);
            }
        }
        false
    }

    /// Parse expression statement: expression;
    pub fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        //println!("in parse_expression_statement");

        let start_pos = self.current;
            
        let expr = self.parse_expression()?;

        //println!("in parse_expression_statement parsed {:#?}", expr);

        // Check for directive prologue
        let is_directive = if let Expression::Literal(Literal::String(_)) = &expr {
            // Only consider as directive if it's at the beginning of a function/program
            // and is a simple string literal (not an expression)
            start_pos == 0 || self.previous().unwrap().token_type == TokenType::LeftBrace
        } else {
            false
        };

        self.consume(&TokenType::Semicolon, "Expected ';' after expression statement")?;
        
        // If this is a "use strict" directive, update parser state
        if is_directive {
            if let Expression::Literal(Literal::String(value)) = &expr {
                if value.as_ref() == "use strict" {
                    self.state.in_strict_mode = true;
                }
            }
        }
        
        Ok(Statement::Expression(expr))
    }


    /// Parse switch statement: switch (discriminant) { case/default... }
    fn parse_switch(&mut self) -> ParseResult<Statement> {  
        self.advance(); // consume 'switch'  
        self.consume(&TokenType::LeftParen, "Expected '(' after 'switch'")?;  
          
        let discriminant = self.parse_expression()?;  
        self.consume(&TokenType::RightParen, "Expected ')' after switch expression")?;  
          
        self.consume(&TokenType::LeftBrace, "Expected '{' before switch cases")?;  
          
        // Use SwitchBody context instead of state flag
        let cases = self.with_context(LexicalContext::SwitchBody, |parser| {
            let mut inner_cases = Vec::new();  
            let mut has_default = false;  
              
            while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {  
                let case = parser.parse_switch_case()?;  
                  
                // Validate only one default case  
                if case.test.is_none() {  
                    if has_default {  
                        return Err(parser_error_at_current!(parser, "Multiple default clauses in switch statement"));
                    }  
                    has_default = true;  
                }  
                  
                inner_cases.push(case);  
            }
            
            Ok(inner_cases)
        })?;
          
        self.consume(&TokenType::RightBrace, "Expected '}' after switch cases")?;  
          
        Ok(Statement::Switch { discriminant, cases })  
    }

    /// Parse while statement: while (test) statement
    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'while'
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        
        // Use LoopBody context instead of state flag
        let body = self.with_context(LexicalContext::LoopBody, |parser| {
            parser.parse_statement().map(Box::new)
        })?;
        
        Ok(Statement::Loop(LoopStatement::While { test, body }))
    }

    /// Parse do-while statement: do statement while (test);
    fn parse_do_while(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'do'
        
        // Use LoopBody context instead of state flag
        let body = self.with_context(LexicalContext::LoopBody, |parser| {
            parser.parse_statement().map(Box::new)
        })?;
        
        self.consume(&TokenType::While, "Expected 'while' after do block")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        self.consume(&TokenType::Semicolon, "Expected ';' after do-while statement")?;
        
        Ok(Statement::Loop(LoopStatement::DoWhile { body, test }))
    }

    /// Parse break statement: break [label];
    fn parse_break(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'break'

        if !self.is_in_loop() && !self.is_in_switch() {
            return Err(parser_error_at_current!(self, "'break' statement outside of loop or switch"));
        }
        
        // Optional label
        let label = if !self.check(&TokenType::Semicolon) && !self.previous_line_terminator() {
            if let Some(TokenType::Identifier(name)) = self.peek_token().map(|t| &t.token_type).cloned() {
                self.advance();
                
                // Verify label exists
                let label_name = name.into_boxed_str();
                if !self.state.labels.contains(&label_name) {
                    return Err(parser_error_at_current!(self, "Undefined label '{}'", label_name));
                }
                
                Some(label_name)
            } else {
                None
            }
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after break statement")?;
        
        Ok(Statement::Break(label))
    }
    
    /// Parse continue statement: continue [label];
    fn parse_continue(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'continue'
         
        // Check if we're in a loop using context method
        if !self.is_in_loop() {
            return Err(parser_error_at_current!(self, "'continue' statement outside of loop"));
        }
        
        // Optional label
        let label = if !self.check(&TokenType::Semicolon) && !self.previous_line_terminator() {
            if let Some(TokenType::Identifier(name)) = self.peek_token().map(|t| &t.token_type).cloned() {
                self.advance();
                
                // Verify label exists
                let label_name = name.into_boxed_str();
                if !self.state.labels.contains(&label_name) {
                    return Err(parser_error_at_current!(self, "Undefined label '{}'", label_name));
                }
                
                Some(label_name)
            } else {
                None
            }
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after continue statement")?;
        
        Ok(Statement::Continue(label))
    }

    /// Parse return statement: return [expression];
    fn parse_return(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'return'
        
         // Check if we're in a function using context method
        if !self.is_in_function() {
            return Err(parser_error_at_current!(self, "'return' statement outside of function"));
        }
        
        // Return with no value if semicolon or end of block
        let argument = (!self.check(&TokenType::Semicolon) && 
                        !self.check(&TokenType::RightBrace) && 
                        !self.is_at_end() && 
                        !self.previous_line_terminator())
            .then(|| self.parse_expression())
            .transpose()?;
        
        self.consume(&TokenType::Semicolon, "Expected ';' after return statement")?;
        
        Ok(Statement::Return(argument))
    }

    /// Parse for statement: for ([init]; [test]; [update]) statement
    fn parse_for(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'for'
        
        // Check for for-await-of
        let is_await = self.match_token(&TokenType::Await);
        
        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse initialization with LoopParameters context
        let result = self.with_context(LexicalContext::LoopParameters, |parser| {
            // ... existing for loop parsing code ...
            if parser.match_token(&TokenType::Semicolon) {
                // No initialization - standard for loop with empty init
                // Parse condition
                let test = (!parser.check(&TokenType::Semicolon))
                    .then(|| parser.parse_expression())
                    .transpose()?;
                    
                parser.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                
                // Parse update
                let update = (!parser.check(&TokenType::RightParen))
                    .then(|| parser.parse_expression())
                    .transpose()?;
                    
                parser.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                
                // Parse body with LoopBody context
                let body = parser.with_context(LexicalContext::LoopBody, |p| {
                    p.parse_statement().map(Box::new)
                })?;
                
                Ok(LoopStatement::For { 
                    init: None, 
                    test, 
                    update, 
                    body 
                })
            } else if parser.check(&TokenType::Var) || parser.check(&TokenType::Let) || parser.check(&TokenType::Const) {
                // Variable declaration initialization
                let decl = parser.parse_variable_declaration()?;
                
                // Check for for-in or for-of
                if parser.check(&TokenType::In) {
                    // for-in loop with variable declaration
                    parser.advance(); // consume 'in'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForIn { 
                        left: ForInOfLeft::Declaration(decl), 
                        right, 
                        body 
                    })
                } else if parser.check(&TokenType::Of) {
                    // for-of loop with variable declaration
                    parser.advance(); // consume 'of'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForOf { 
                        left: ForInOfLeft::Declaration(decl), 
                        right, 
                        body, 
                        is_await 
                    })
                } else {
                    // Standard for loop with variable declaration
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                    
                    // Parse condition
                    let test = (!parser.check(&TokenType::Semicolon))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                    
                    // Parse update
                    let update = (!parser.check(&TokenType::RightParen))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::For { 
                        init: Some(ForInit::Variable(decl)), 
                        test, 
                        update, 
                        body 
                    })
                }
            } else if let Some(TokenType::Identifier(_)) = parser.peek_token_type() {
                // ... existing identifier handling code ...
                // First, check if the next tokens form a for-in or for-of loop
                // Save current position to backtrack if needed
                let start_pos = parser.current;
                
                // Parse the identifier
                let token = parser.advance().unwrap().clone();
                let name = parser.expect_identifier("Expected label name")?;
                let left = Expression::Identifier(name);
                
                // Check what follows the identifier
                if parser.check(&TokenType::In) {
                    // for-in loop with identifier
                    parser.advance(); // consume 'in'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForIn { 
                        left: ForInOfLeft::Pattern(left), 
                        right, 
                        body 
                    })
                } else if parser.check(&TokenType::Of) {
                    // for-of loop with identifier
                    parser.advance(); // consume 'of'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForOf { 
                        left: ForInOfLeft::Pattern(left), 
                        right,
                        body, 
                        is_await 
                    })
                } else {
                    // Not a for-in or for-of loop, so it must be a standard for loop
                    // Reset position and parse the full initialization expression
                    parser.current = start_pos;
                    
                    // Parse the initialization expression
                    let init_expr = parser.parse_expression()?;
                    
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                    
                    // Parse condition
                    let test = (!parser.check(&TokenType::Semicolon))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                    
                    // Parse update (which might be empty)
                    let update = (!parser.check(&TokenType::RightParen))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::For { 
                        init: Some(ForInit::Expression(init_expr)), 
                        test, 
                        update, 
                        body 
                    })
                }
            } else {
                // For other expressions (including array/object literals and complex expressions)
                // Parse the full initialization expression
                let init_expr = parser.parse_expression()?;
                
                // Check if this is a for-in or for-of loop
                if parser.check(&TokenType::In) {
                    // for-in loop with expression
                    parser.advance(); // consume 'in'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForIn { 
                        left: ForInOfLeft::Pattern(init_expr), 
                        right, 
                        body 
                    })
                } else if parser.check(&TokenType::Of) {
                    // for-of loop with expression
                    parser.advance(); // consume 'of'
                    let right = parser.parse_expression()?;
                    parser.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::ForOf { 
                        left: ForInOfLeft::Pattern(init_expr), 
                        right, 
                        body, 
                        is_await 
                    })
                } else {
                    // Standard for loop with expression initialization
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                    
                    // Parse condition
                    let test = (!parser.check(&TokenType::Semicolon))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                    
                    // Parse update (which might be empty)
                    let update = (!parser.check(&TokenType::RightParen))
                        .then(|| parser.parse_expression())
                        .transpose()?;
                        
                    parser.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                    
                    // Parse body with LoopBody context
                    let body = parser.with_context(LexicalContext::LoopBody, |p| {
                        p.parse_statement().map(Box::new)
                    })?;
                    
                    Ok(LoopStatement::For { 
                        init: Some(ForInit::Expression(init_expr)), 
                        test, 
                        update, 
                        body 
                    })
                }
            }
        })?;
        
        Ok(Statement::Loop(result))
    }


    /// Parse a variable declarator: pattern = initializer
    pub fn parse_variable_declarator(&mut self) -> ParseResult<VariableDeclarator> {
        // Get the current token position for error reporting
        let start_token = self.peek_token().cloned();
        
        // Parse the binding pattern (identifier, object pattern, or array pattern)
        let id = self.parse_pattern()?;
        
        // Check if this is a const declaration without an initializer
        let is_const = if let Some(prev_token) = self.tokens.get(self.current - 2) {
            matches!(prev_token.token_type, TokenType::Const)
        } else {
            false
        };
        
        // Parse optional initializer
        let init = if self.match_token(&TokenType::Equal) {
            // Parse the initializer expression
            Some(self.parse_expression()?)
        } else {
            // Const declarations must have initializers
            if is_const {
                if let Some(token) = start_token {
                    return Err(parser_error_at_current!(self, "Missing initializer in const declaration"));
                } else {
                    return Err(parser_error_at_current!(self, "Missing initializer in const declaration"));
                }
            }
            None
        };
        
        // Validate the pattern based on the current context
        self.validate_binding_pattern(&id, is_const)?;
        
        Ok(VariableDeclarator { id, init })
    }

    /// Helper method to validate a binding pattern
    fn validate_binding_pattern(&self, pattern: &Expression, is_const: bool) -> ParseResult<()> {
    match pattern {
        // For simple identifiers, check for strict mode restrictions
        Expression::Identifier(name) => {
            // Check for reserved words in strict mode
            if self.state.in_strict_mode {
                let reserved_words = ["eval", "arguments"];
                if reserved_words.contains(&name.as_ref()) {
                    return Err(parser_error_at_current!(self, "'{}' cannot be used as a variable name in strict mode", name));
                }
            }
            
            // Check for other JavaScript reserved words that can't be variable names
            let always_reserved = ["let", "yield", "await", "static", "implements", 
                                  "interface", "package", "private", "protected", "public"];
            if always_reserved.contains(&name.as_ref()) {
                return Err(parser_error_at_current!(self, "'{}' is a reserved word and cannot be used as a variable name", name));
            }
        },
        // For object patterns, recursively validate each property
        Expression::Object(properties) => {
            for property in properties {
                match property {
                    ObjectProperty::Property { key, value, .. } => {
                        // Validate the value part of the property
                        self.validate_binding_pattern(value, is_const)?;
                    },
                    ObjectProperty::Spread(expr) => {
                        // For spread elements, validate the spread target
                        if let Expression::Identifier(name) = expr {
                            self.validate_binding_pattern(&Expression::Identifier(name.clone()), is_const)?;
                        } else if let Expression::Object(_) | Expression::Array(_) = expr {
                            self.validate_binding_pattern(expr, is_const)?;
                        } else {
                            return Err(parser_error_at_current!(self, "Invalid rest element in object pattern"));
                        }
                    },
                    _ => {
                        // Methods are not allowed in binding patterns
                        return Err(parser_error_at_current!(self, "Method definitions are not allowed in object patterns"));
                    }
                }
            }
        },
        // For array patterns, recursively validate each element
        Expression::Array(elements) => {
            for element in elements {
                match element {
                    ArrayElement::Expression(expr) => {
                        self.validate_binding_pattern(expr, is_const)?;
                    },
                    ArrayElement::Spread(expr) => {
                        // For spread elements, validate the spread target
                        if let Expression::Identifier(name) = expr {
                            self.validate_binding_pattern(&Expression::Identifier(name.clone()), is_const)?;
                        } else if let Expression::Object(_) | Expression::Array(_) = expr {
                            self.validate_binding_pattern(expr, is_const)?;
                        } else {
                            return Err(parser_error_at_current!(self, "Invalid rest element in array pattern"));
                        }
                    },
                    ArrayElement::Hole => {
                        // Holes are allowed in array patterns
                    }
                }
            }
        },
        // Handle assignment patterns (default values)
        Expression::Assignment { left, .. } => {
            self.validate_binding_pattern(left, is_const)?;
        },
        // Handle spread elements
        Expression::Spread(inner) => {
            self.validate_binding_pattern(inner, is_const)?;
        },
        // Other expression types are not valid binding patterns
        _ => {
            return Err(parser_error_at_current!(self, "Invalid binding pattern in variable declaration"));
        }
    }
    
    Ok(())
}


}
