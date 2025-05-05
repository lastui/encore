use crate::ast::*;
use crate::lexer::TokenType;
use super::error::{ParserError, ParseResult};
use super::core::Parser;

impl Parser {

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

    /// Parse switch statement: switch (discriminant) { case/default... }
    fn parse_switch(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'switch'
        self.consume(&TokenType::LeftParen, "Expected '(' after 'switch'")?;
        
        let discriminant = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after switch expression")?;
        self.consume(&TokenType::LeftBrace, "Expected '{' to start switch block")?;
        
        // Save previous switch state
        let prev_in_switch = self.state.in_switch;
        self.state.in_switch = true;
        
        let mut cases = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            cases.push(self.parse_switch_case()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' to end switch block")?;
        
        // Restore previous switch state
        self.state.in_switch = prev_in_switch;
        
        Ok(Statement::Switch { discriminant, cases })
    }

    /// Parse a single case in a switch statement
    fn parse_switch_case(&mut self) -> ParseResult<SwitchCase> {
        let test = if self.match_token(&TokenType::Case) {
            Some(self.parse_expression()?)
        } else if self.match_token(&TokenType::Default) {
            None
        } else {
            return Err(self.error_unexpected("Expected 'case' or 'default'"));
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

    /// Parse while statement: while (test) statement
    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'while'
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        
        // Save previous loop state
        let prev_in_loop = self.state.in_loop;
        self.state.in_loop = true;
        
        let body = Box::new(self.parse_statement()?);
        
        // Restore previous loop state
        self.state.in_loop = prev_in_loop;
        
        Ok(Statement::Loop(LoopStatement::While { test, body }))
    }

    /// Parse do-while statement: do statement while (test);
    fn parse_do_while(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'do'
        
        // Save previous loop state
        let prev_in_loop = self.state.in_loop;
        self.state.in_loop = true;
        
        let body = Box::new(self.parse_statement()?);
        
        // Restore previous loop state
        self.state.in_loop = prev_in_loop;
        
        self.consume(&TokenType::While, "Expected 'while' after do block")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        self.consume(&TokenType::Semicolon, "Expected ';' after do-while statement")?;
        
        Ok(Statement::Loop(LoopStatement::DoWhile { body, test }))
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
            return Err(self.error_unexpected("Expected 'catch' or 'finally' after try block"));
        }
        
        Ok(Statement::Try { block, handler, finalizer })
    }

    /// Parse catch clause: catch ([param]) block
    fn parse_catch_clause(&mut self) -> ParseResult<CatchClause> {
        // Optional catch parameter
        let param = self.match_token(&TokenType::LeftParen)
            .then(|| {
                //let param = self.parse_pattern()?;
                // TODO fixme
                let param = None;
                self.consume(&TokenType::RightParen, "Expected ')' after catch parameter")?;
                // Explicitly specify the error type as ParserError
                Ok::<_, super::error::ParserError>(param)
            })
            .transpose()?;
        
        let body = Box::new(self.parse_block()?);
        
        Ok(CatchClause { param: param.expect("REASON"), body })
    }

    /// Parse throw statement: throw expression;
    fn parse_throw(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'throw'
        
        // No line terminator allowed between throw and expression
        if self.previous_line_terminator() {
            return Err(ParserError::with_token_span(
                "Illegal newline after throw",
                token.line,
                token.column,
                token.length,
                &self.get_source_text(),
            ));
        }
        
        let expr = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after throw statement")?;
        
        Ok(Statement::Throw(expr))
    }

    /// Parse return statement: return [expression];
    fn parse_return(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'return'
        
        // Check if we're in a function
        if !self.state.in_function {
            return Err(ParserError::with_token_span(
                "'return' statement outside of function",
                token.line,
                token.column,
                token.length,
                &self.get_source_text(),
            ));
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

    /// Parse break statement: break [label];
    fn parse_break(&mut self) -> ParseResult<Statement> {
        let token = self.advance().cloned().unwrap(); // consume 'break'
        
        // Check if we're in a loop or switch
        if !self.state.in_loop && !self.state.in_switch {
            return Err(ParserError::with_token_span(
                "'break' statement outside of loop or switch",
                token.line,
                token.column,
                token.length,
                &self.get_source_text(),
            ));
        }
        
        // Optional label
        let label = if !self.check(&TokenType::Semicolon) && !self.previous_line_terminator() {
            if let Some(TokenType::Identifier(name)) = self.peek_token().map(|t| &t.token_type).cloned() {
                self.advance();
                
                // Verify label exists
                let label_name = name.into_boxed_str();
                if !self.state.labels.contains(&label_name) {
                    return Err(ParserError::with_token_span(
                        &format!("Undefined label '{}'", label_name),
                        token.line,
                        token.column,
                        token.length,
                        &self.get_source_text(),
                    ));
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
        
        // Check if we're in a loop
        if !self.state.in_loop {
            return Err(ParserError::with_token_span(
                "'continue' statement outside of loop",
                token.line,
                token.column,
                token.length,
                &self.get_source_text(),
            ));
        }
        
        // Optional label
        let label = if !self.check(&TokenType::Semicolon) && !self.previous_line_terminator() {
            if let Some(TokenType::Identifier(name)) = self.peek_token().map(|t| &t.token_type).cloned() {
                self.advance();
                
                // Verify label exists
                let label_name = name.into_boxed_str();
                if !self.state.labels.contains(&label_name) {
                    return Err(ParserError::with_token_span(
                        &format!("Undefined label '{}'", label_name),
                        token.line,
                        token.column,
                        token.length,
                        &self.get_source_text(),
                    ));
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

    /// Parse with statement: with (object) statement
    fn parse_with(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'with'
        
        // Check if in strict mode
        if self.state.in_strict_mode {
            return Err(ParserError::with_token_span(
                "'with' statements are not allowed in strict mode", 
                self.previous().unwrap().line, 
                self.previous().unwrap().column,
                self.previous().unwrap().length,
                &self.get_source_text(),
            ));
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
        let label = self.identifier_name(&token)?;
        
        self.consume(&TokenType::Colon, "Expected ':' after label")?;
        
        // Add label to the set of active labels
        let label_exists = !self.state.labels.insert(label.clone());
        if label_exists {
            return Err(ParserError::with_token_span(
                &format!("Label '{}' has already been declared", label), 
                token.line, 
                token.column,
                token.length,
                &self.get_source_text(),
            ));
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
        // Handle directives (like "use strict")
        let start_pos = self.current;
            
        //println!("Before parse expression");
        let expr = self.parse_expression()?;

        //println!("After parse expression: {:#?}", expr);

        // Check for directive prologue
        let is_directive = if let Expression::Literal(Literal::String(_)) = &expr {
            //println!("This case");
            // Only consider as directive if it's at the beginning of a function/program
            // and is a simple string literal (not an expression)
            start_pos == 0 || self.previous().unwrap().token_type == TokenType::LeftBrace
        } else {
            //println!("That case");
            false
        };
        
        //println!("now need a ;");


        //if self.check(&TokenType::)
        //if self.check(&TokenType::LeftParen) {
            //println!("Immediately invoked?");
        //}


        //println!(self.peek_token_type())
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

    /// Parse for statement: for ([init]; [test]; [update]) statement
    fn parse_for(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'for'
        
        // Check for for-await-of
        let is_await = self.match_token(&TokenType::Await);
        
        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'")?;
        
        // Save previous loop state
        let prev_in_loop = self.state.in_loop;
        self.state.in_loop = true;
        
        // Parse initialization
        let result = if self.match_token(&TokenType::Semicolon) {
            // No initialization - standard for loop with empty init
            // Parse condition
            let test = (!self.check(&TokenType::Semicolon))
                .then(|| self.parse_expression())
                .transpose()?;
                
            self.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
            
            // Parse update
            let update = (!self.check(&TokenType::RightParen))
                .then(|| self.parse_expression())
                .transpose()?;
                
            self.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
            
            // Parse body
            let body = Box::new(self.parse_statement()?);
            
            LoopStatement::For { 
                init: None, 
                test, 
                update, 
                body 
            }
        } else if self.check(&TokenType::Var) || self.check(&TokenType::Let) || self.check(&TokenType::Const) {
            // Variable declaration initialization
            let decl = self.parse_variable_declaration()?;
            
            // Check for for-in or for-of
            if self.check(&TokenType::In) {
                // for-in loop with variable declaration
                self.advance(); // consume 'in'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForIn { 
                    left: ForInOfLeft::Declaration(decl), 
                    right, 
                    body 
                }
            } else if self.check(&TokenType::Of) {
                // for-of loop with variable declaration
                self.advance(); // consume 'of'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForOf { 
                    left: ForInOfLeft::Declaration(decl), 
                    right, 
                    body, 
                    is_await 
                }
            } else {
                // Standard for loop with variable declaration
                self.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                
                // Parse condition
                let test = (!self.check(&TokenType::Semicolon))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                
                // Parse update
                let update = (!self.check(&TokenType::RightParen))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                
                // Parse body
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::For { 
                    init: Some(ForInit::Variable(decl)), 
                    test, 
                    update, 
                    body 
                }
            }
        } else if let Some(TokenType::Identifier(_)) = self.peek_token_type() {
            // For identifiers, we need to check if they're followed by 'in', 'of', or other tokens
            // that would indicate different types of for loops
            
            // First, check if the next tokens form a for-in or for-of loop
            // Save current position to backtrack if needed
            let start_pos = self.current;
            
            // Parse the identifier
            let token = self.advance().unwrap().clone();
            let name = self.identifier_name(&token)?;
            let left = Expression::Identifier(name);

            // Check what follows the identifier
            if self.check(&TokenType::In) {
                // for-in loop with identifier
                self.advance(); // consume 'in'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForIn { 
                    left: ForInOfLeft::Pattern(left), 
                    right, 
                    body 
                }
            } else if self.check(&TokenType::Of) {
                // for-of loop with identifier
                self.advance(); // consume 'of'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForOf { 
                    left: ForInOfLeft::Pattern(left), 
                    right, 
                    body, 
                    is_await 
                }
            } else {
                // Not a for-in or for-of loop, so it must be a standard for loop
                // Reset position and parse the full initialization expression
                self.current = start_pos;
                
                // Parse the initialization expression
                let init_expr = self.parse_expression()?;
                
                self.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                
                // Parse condition
                let test = (!self.check(&TokenType::Semicolon))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                
                // Parse update (which might be empty)
                let update = (!self.check(&TokenType::RightParen))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                
                // Parse body
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::For { 
                    init: Some(ForInit::Expression(init_expr)), 
                    test, 
                    update, 
                    body 
                }
            }
        } else {
            // For other expressions (including array/object literals and complex expressions)
            // Parse the full initialization expression
            let init_expr = self.parse_expression()?;
            
            // Check if this is a for-in or for-of loop
            if self.check(&TokenType::In) {
                // for-in loop with expression
                self.advance(); // consume 'in'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-in right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForIn { 
                    left: ForInOfLeft::Pattern(init_expr), 
                    right, 
                    body 
                }
            } else if self.check(&TokenType::Of) {
                // for-of loop with expression
                self.advance(); // consume 'of'
                let right = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after for-of right-hand side")?;
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::ForOf { 
                    left: ForInOfLeft::Pattern(init_expr), 
                    right, 
                    body, 
                    is_await 
                }
            } else {
                // Standard for loop with expression initialization
                self.consume(&TokenType::Semicolon, "Expected ';' after for initialization")?;
                
                // Parse condition
                let test = (!self.check(&TokenType::Semicolon))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
                
                // Parse update (which might be empty)
                let update = (!self.check(&TokenType::RightParen))
                    .then(|| self.parse_expression())
                    .transpose()?;
                    
                self.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
                
                // Parse body
                let body = Box::new(self.parse_statement()?);
                
                LoopStatement::For { 
                    init: Some(ForInit::Expression(init_expr)), 
                    test, 
                    update, 
                    body 
                }
            }
        };
        
        // Restore previous loop state
        self.state.in_loop = prev_in_loop;
        
        Ok(Statement::Loop(result))
    }

}
