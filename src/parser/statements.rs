use super::prelude::*;

use crate::ast::*;
use crate::lexer::{Token, LexicalContext};
use super::error::ParseResult;
use super::core::Parser;

impl Parser {

   // Variable declarations
    pub fn parse_variable_declaration(&mut self) -> ParseResult<VariableDeclaration> {
        
        let kind = match self.advance() {
            Some(Token::Var) => VariableKind::Var,
            Some(Token::Let) => VariableKind::Let,
            Some(Token::Const) => VariableKind::Const,
            _ => unreachable!(),
        };
        
        // Parse first declarator (required)
        let mut declarations = vec![self.parse_variable_declarator()?];
        
        // Parse additional declarators separated by commas
        while self.match_token(&Token::Comma) {
            declarations.push(self.parse_variable_declarator()?);
        }

        // Consume semicolon unless we're in a for-in/of loop context
        //let current_context = self.current_context();
        //let is_in_loop_parameters = matches!(current_context, LexicalContext::LoopParameters);


        if !self.is_in_loop_parameters() {
            self.consume(&Token::Semicolon, "Expected ';' after variable declaration")?;
        }
        
        Ok(VariableDeclaration { declarations, kind })
    }


    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            // Empty statement (just a semicolon)
            Some(Token::Semicolon) => {
                self.advance();
                Ok(Statement::Empty)
            },

            // Block statement or literal object expression { ... } 
            Some(Token::LeftBrace) => {
                match self.peek_previous() {
                    Some(Token::RightParen) => self.parse_block(),
                    _ => match self.peek_next(1) {
                        Some(Token::RightBrace) | Some(Token::LeftBracket) | Some(Token::Ellipsis) => self.parse_expression_statement(),
                        Some(Token::Identifier(_)) | Some(Token::StringLiteral(_)) => {
                            if let Some(Token::Colon) = self.peek_next(2) {
                                self.parse_expression_statement()
                            } else {
                                self.parse_block()
                            }
                        }
                        _ => self.parse_block(),
                    },
                }
            },
            
            // Declaration statements
            Some(Token::Var) | Some(Token::Let) | Some(Token::Const) => self.parse_variable_statement(),
            Some(Token::Function) =>  self.parse_function_statement(),
            Some(Token::Class) =>  self.parse_class_statement(),
            
            // Control flow statements
            Some(Token::If) => self.parse_if(),
            Some(Token::Switch) => self.parse_switch(),
            Some(Token::For) => self.parse_for(),
            Some(Token::While) => self.parse_while(),
            Some(Token::Do) => self.parse_do_while(),
            
            // Exception handling
            Some(Token::Try) => self.parse_try(),
            Some(Token::Throw) => self.parse_throw(),
            
            // Function control
            Some(Token::Return) => self.parse_return(),
            Some(Token::Break) => self.parse_break(),
            Some(Token::Continue) => self.parse_continue(),
            
            // Module statements
            Some(Token::Import) => self.parse_import_statement(),
            Some(Token::Export) => self.parse_export_statement(),
            
            // Other statements
            Some(Token::With) => self.parse_with(),
            Some(Token::Debugger) => self.parse_debugger(),
            
            // Labeled statement
            Some(Token::Identifier(_)) if self.is_label() => self.parse_labeled(),
            
            // Default: expression statement
            _ => self.parse_expression_statement(),
        }
    }

    /// Parse a block statement: { statements... }
    fn parse_block(&mut self) -> ParseResult<Statement> {
        self.consume(&Token::LeftBrace, "Expected '{'")?;
        
        let mut statements = Vec::new();
        
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        
        self.consume(&Token::RightBrace, "Expected '}'")?;
        
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
        self.consume(&Token::LeftParen, "Expected '(' after 'if'")?;
        
        let test = self.parse_expression()?;
        self.consume(&Token::RightParen, "Expected ')' after if condition")?;
        
        let consequent = Box::new(self.parse_statement()?);
        let alternate = self.match_token(&Token::Else)
            .then(|| self.parse_statement().map(Box::new))
            .transpose()?;
        
        Ok(Statement::If { test, consequent, alternate })
    }

    /// Parse a single case in a switch statement
    fn parse_switch_case(&mut self) -> ParseResult<SwitchCase> {
        let test = if self.match_token(&Token::Case) {
            // After 'case', we expect an expression
            Some(self.parse_expression()?)
        } else if self.match_token(&Token::Default) {
            None
        } else {
            println!("Current token {:#?}", self.peek());
            return Err(parser_error_at_current!(self, "Expected 'case' or 'default'"));
        };
        
        self.consume(&Token::Colon, "Expected ':' after case value")?;
        
        let mut consequent = Vec::new();
        
        // Parse statements until next case, default, or end of switch
        while !self.check(&Token::Case) && 
              !self.check(&Token::Default) && 
              !self.check(&Token::RightBrace) && 
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
        let handler = self.match_token(&Token::Catch)
            .then(|| self.parse_catch_clause())
            .transpose()?;
        
        // Parse optional finally clause
        let finalizer = self.match_token(&Token::Finally)
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
        let param = self.match_token(&Token::LeftParen)
            .then(|| {
                // Attempt to parse the parameter identifier
                if let Some(Token::Identifier(name)) = self.peek().cloned() {
                    self.advance(); // Consume the identifier
                    self.consume(&Token::RightParen, "Expected ')' after catch parameter")?;
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
        self.advance(); // consume 'throw'
        
        // No line terminator allowed between throw and expression
        if self.previous_line_terminator() {
            return Err(parser_error_at_current!(self, "Illegal newline after throw"));
        }
        
        let expr = self.parse_expression()?;
        self.consume(&Token::Semicolon, "Expected ';' after throw statement")?;
        
        Ok(Statement::Throw(expr))
    }

    /// Parse with statement: with (object) statement
    fn parse_with(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'with'
        
        // Check if in strict mode
        if self.state.in_strict_mode {
            return Err(parser_error_at_current!(self, "'with' statements are not allowed in strict mode"));
        }
        
        self.consume(&Token::LeftParen, "Expected '(' after 'with'")?;
        
        let object = self.parse_expression()?;
        
        self.consume(&Token::RightParen, "Expected ')' after with expression")?;
        
        let body = Box::new(self.parse_statement()?);
        
        Ok(Statement::With { object, body })
    }

    /// Parse debugger statement: debugger;
    fn parse_debugger(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'debugger'
        
        self.consume(&Token::Semicolon, "Expected ';' after debugger statement")?;
        
        Ok(Statement::Debugger)
    }

    /// Parse labeled statement: identifier: statement
    fn parse_labeled(&mut self) -> ParseResult<Statement> {
        let label = self.expect_identifier("Expected label name")?;
        
        self.consume(&Token::Colon, "Expected ':' after label")?;
        
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
        if let Some(Token::Identifier(_)) = self.peek() {
            if let Some(next_token) = self.peek_next(1) {
                return matches!(next_token, Token::Colon);
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
            start_pos == 0 || self.peek_previous().unwrap() == &Token::LeftBrace
        } else {
            false
        };

        if !self.is_in_loop_parameters() {
            self.consume(&Token::Semicolon, "Expected ';' after expression statement")?;
        }

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
        self.consume(&Token::LeftParen, "Expected '(' after 'switch'")?;  
          
        let discriminant = self.parse_expression()?;  
        self.consume(&Token::RightParen, "Expected ')' after switch expression")?;  
          
        self.consume(&Token::LeftBrace, "Expected '{' before switch cases")?;  
          
        // Use SwitchBody context instead of state flag
        let cases = self.with_context(LexicalContext::SwitchBody, |parser| {
            let mut inner_cases = Vec::new();  
            let mut has_default = false;  
              
            while !parser.check(&Token::RightBrace) && !parser.is_at_end() {  
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
          
        self.consume(&Token::RightBrace, "Expected '}' after switch cases")?;  
          
        Ok(Statement::Switch { discriminant, cases })  
    }

    /// Parse while statement: while (test) statement
    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'while'
        self.consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        self.consume(&Token::RightParen, "Expected ')' after while condition")?;
        
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
        
        self.consume(&Token::While, "Expected 'while' after do block")?;
        self.consume(&Token::LeftParen, "Expected '(' after 'while'")?;
        
        let test = self.parse_expression()?;
        
        self.consume(&Token::RightParen, "Expected ')' after while condition")?;
        self.consume(&Token::Semicolon, "Expected ';' after do-while statement")?;
        
        Ok(Statement::Loop(LoopStatement::DoWhile { body, test }))
    }

    /// Parse break statement: break [label];
    fn parse_break(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'break'

        if !self.is_in_loop_body() && !self.is_in_switch() {
            return Err(parser_error_at_current!(self, "'break' statement outside of loop or switch"));
        }
        
        // Optional label
        let label = if !self.check(&Token::Semicolon) && !self.previous_line_terminator() {
            if let Some(Token::Identifier(name)) = self.peek().cloned() {
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
        
        self.consume(&Token::Semicolon, "Expected ';' after break statement")?;
        
        Ok(Statement::Break(label))
    }
    
    /// Parse continue statement: continue [label];
    fn parse_continue(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'continue'
         
        // Check if we're in a loop using context method
        if !self.is_in_loop_body() {
            return Err(parser_error_at_current!(self, "'continue' statement outside of loop"));
        }
        
        // Optional label
        let label = if !self.check(&Token::Semicolon) && !self.previous_line_terminator() {
            if let Some(Token::Identifier(name)) = self.peek().cloned() {
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
        
        self.consume(&Token::Semicolon, "Expected ';' after continue statement")?;
        
        Ok(Statement::Continue(label))
    }

    /// Parse return statement: return [expression];
    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'return'
        
         // Check if we're in a function using context method
        if !self.is_in_function() {
            return Err(parser_error_at_current!(self, "'return' statement outside of function"));
        }
        
        // Return with no value if semicolon or end of block
        let argument = (!self.check(&Token::Semicolon) && 
                        !self.check(&Token::RightBrace) && 
                        !self.is_at_end() && 
                        !self.previous_line_terminator())
            .then(|| self.parse_expression())
            .transpose()?;
        
        self.consume(&Token::Semicolon, "Expected ';' after return statement")?;
        
        Ok(Statement::Return(argument))
    }

    /// Parse for statement: for ([init]; [test]; [update]) statement
    fn parse_for(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume 'for'
        
        //println!("In for loop construct");

        // Check for for-await-of
        let is_await = self.match_token(&Token::Await);
        
        self.consume(&Token::LeftParen, "Expected '(' after 'for'")?;
        
        let result = self.with_context(LexicalContext::LoopParameters, |parser| {
            if parser.check(&Token::Semicolon) {
                parser.consume(&Token::Semicolon, "Expected ';' after for init")?;

                let test = (!parser.check(&Token::Semicolon))
                    .then(|| parser.parse_expression())
                    .transpose()?;

                parser.consume(&Token::Semicolon, "Expected ';' after for test")?;

                let update = (!parser.check(&Token::Semicolon))
                    .then(|| parser.parse_expression())
                    .transpose()?;

                parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                let body = parser.with_context(LexicalContext::LoopBody, |p| {
                    p.parse_statement().map(Box::new)
                })?;

                Ok(LoopStatement::For { 
                    init: None, 
                    test, 
                    update, 
                    body 
                })
            } else if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {

                //println!("- with initialization");

                let init = parser.parse_variable_declaration()?;

                match parser.advance() {
                    Some(&Token::In) => {
                        let right = parser.parse_expression()?;
                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;
                        Ok(LoopStatement::ForIn { 
                            left: ForInit::Declaration(init), 
                            right, 
                            body 
                        })
                    },
                    Some(&Token::Of) => {
                        let right = parser.parse_expression()?;
                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;

                        Ok(LoopStatement::ForOf { 
                            left: ForInit::Declaration(init), 
                            right, 
                            body,
                            is_await 
                        })
                    },
                    _ => {

                        //println!("- as classical loop");

                        let test = (!parser.check(&Token::Semicolon))
                            .then(|| parser.parse_expression())
                            .transpose()?;

                        parser.consume(&Token::Semicolon, "Expected ';' after for test")?;

                        let update = (!parser.check(&Token::RightParen))
                            .then(|| parser.parse_expression())
                            .transpose()?;

                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;

                        Ok(LoopStatement::For { 
                            init: Some(ForInit::Declaration(init)), 
                            test, 
                            update, 
                            body 
                        })
                    },
                }

            } else if let Some(Token::Identifier(_)) = parser.peek() {
                
                match parser.peek_next(1) {
                    Some(&Token::In) => {
                        let left = Expression::Identifier(parser.expect_identifier("Expected for init name")?);

                        parser.advance();   // consume 'in'

                        let right = parser.parse_expression()?;
                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;
                        Ok(LoopStatement::ForIn { 
                            left: ForInit::Pattern(left), 
                            right, 
                            body 
                        })
                    },
                    Some(&Token::Of) => {
                        let left = Expression::Identifier(parser.expect_identifier("Expected for init name")?);

                        parser.advance();   // consume 'in'

                        let right = parser.parse_expression()?;
                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;
                        Ok(LoopStatement::ForOf { 
                            left: ForInit::Pattern(left), 
                            right, 
                            body,
                            is_await 
                        })
                    },
                    _ => {
                        //println!("classical for loop without initialisation");

                        //println!("current token is {:#?}", parser.peek());

                        let init = (!parser.check(&Token::Semicolon))
                            .then(|| parser.parse_expression())
                            .transpose()?;

                        parser.consume(&Token::Semicolon, "Expected ';' after for init")?;

                        let test = (!parser.check(&Token::Semicolon))
                            .then(|| parser.parse_expression())
                            .transpose()?;

                        parser.consume(&Token::Semicolon, "Expected ';' after for test")?;

                        //println!("Before I am here on {:#?}", parser.peek());

                        let update = (!parser.check(&Token::RightParen))
                            .then(|| parser.parse_expression())
                            .transpose()?;


                        //println!("After I am here on {:#?}", parser.peek());

                        parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                        let body = parser.with_context(LexicalContext::LoopBody, |p| {
                            p.parse_statement().map(Box::new)
                        })?;

                        Ok(LoopStatement::For { 
                            init: init.map(|exp| ForInit::Pattern(exp)), 
                            test, 
                            update, 
                            body 
                        })
                    },
                }
            } else {
                //println!("What did happen? {:#?}", parser.peek());
                //Err(parser_error_at_current!(parser, "unknown for construct"))


                let init = (!parser.check(&Token::Semicolon))
                    .then(|| parser.parse_expression())
                    .transpose()?;

                parser.consume(&Token::Semicolon, "Expected ';' after for init")?;

                let test = (!parser.check(&Token::Semicolon))
                    .then(|| parser.parse_expression())
                    .transpose()?;

                parser.consume(&Token::Semicolon, "Expected ';' after for test")?;

                //println!("Before I am here on {:#?}", parser.peek());

                let update = (!parser.check(&Token::RightParen))
                    .then(|| parser.parse_expression())
                    .transpose()?;


                //println!("After I am here on {:#?}", parser.peek());

                parser.consume(&Token::RightParen, "Expected ')' after 'for'")?;
                let body = parser.with_context(LexicalContext::LoopBody, |p| {
                    p.parse_statement().map(Box::new)
                })?;

                Ok(LoopStatement::For { 
                    init: init.map(|exp| ForInit::Pattern(exp)), 
                    test, 
                    update, 
                    body 
                })
            }

        })?;

        Ok(Statement::Loop(result))

    }

    /// Parse a variable declarator: pattern = initializer
    pub fn parse_variable_declarator(&mut self) -> ParseResult<VariableDeclarator> {
        // Get the current token position for error reporting
        
        let is_const = matches!(self.peek(), Some(&Token::Const));

        
        //println!("1 Now at {:#?}", self.peek());

        // Parse the binding pattern (identifier, object pattern, or array pattern)
        let id = self.parse_pattern()?;
        
        // Check if this is a const declaration without an initializer
        // TODO fix self.tokens direct access
        
        //println!("2 Now at {:#?}", self.peek());

        // Parse optional initializer
        let init = if self.match_token(&Token::Equal) {
            // Parse the initializer expression
            Some(self.parse_expression()?)
        } else {
            // Const declarations must have initializers
            if is_const {
                return Err(parser_error_at_current!(self, "Missing initializer in const declaration"));
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
