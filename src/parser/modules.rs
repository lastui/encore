use crate::ast::*;
use crate::lexer::{Token, TokenType};
use super::error::ParseResult;
use super::core::Parser;

impl Parser {
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut body = Vec::new();
        
        while !self.is_at_end() {
            body.push(self.parse_statement()?);
        }
        
        // Extract comments
        let comments = std::mem::take(&mut self.comments);
        
        Ok(Program {
            source_type: SourceType::Script,
            body,
            comments,
        })
    }

    pub fn parse_module(&mut self) -> ParseResult<Program> {
        // Set strict mode for modules
        self.state.in_strict_mode = true;
        
        let mut body = Vec::new();
        
        while !self.is_at_end() {
            body.push(self.parse_statement()?);
        }
        
        // Extract comments
        let comments = std::mem::take(&mut self.comments);
        
        Ok(Program {
            source_type: SourceType::Module,
            body,
            comments,
        })
    }

    pub fn parse_import_statement(&mut self) -> ParseResult<Statement> {
        let start_token = self.advance().unwrap(); // consume 'import'
        
        // Handle import() expression vs import statement
        if self.check(&TokenType::LeftParen) {
            // This is an import() expression, not an import statement
            // Rewind and parse as expression statement
            self.current -= 1;
            return self.parse_expression_statement();
        }
        
        let mut specifiers = Vec::new();
        let mut source: Option<Box<str>> = None;
        
        // Handle different import forms
        if matches!(self.peek_token_type(), Some(TokenType::StringLiteral(_))) {
            // import "module-name"; (side-effect import)
            source = self.parse_module_source()?;
        } else if self.match_token(&TokenType::Star) {
            // import * as name from "module-name"; (namespace import)
            specifiers.push(self.parse_namespace_import()?);
            source = self.parse_from_clause()?;
        } else {
            // import defaultExport, { named1, named2 } from "module-name";
            // or just { named1, named2 } from "module-name";
            
            // Check for default import
            if !self.check(&TokenType::LeftBrace) && !self.check(&TokenType::From) {
                specifiers.push(self.parse_default_import()?);
                
                // Optional comma before named imports
                if self.match_token(&TokenType::Comma) && !self.check(&TokenType::From) {
                    // Continue to named imports
                } else if !self.check(&TokenType::From) {
                    // If no comma and not 'from', it's an error
                    return Err(self.error_unexpected("Expected ',' or 'from' after default import"));
                }
            }
            
            // Named imports
            if self.match_token(&TokenType::LeftBrace) {
                let named_imports = self.parse_named_imports()?;
                specifiers.extend(named_imports);
            }
            
            // Module source
            if !specifiers.is_empty() {
                source = self.parse_from_clause()?;
            } else {
                return Err(self.error_unexpected("Expected import specifiers"));
            }
        }
        
        // Parse import assertions if present
        let assertions = if self.match_token(&TokenType::With) {
            self.parse_import_assertions()?
        } else {
            Vec::new()
        };
        
        self.consume_semicolon("Expected ';' after import statement")?;
        
        if let Some(src) = source {
            Ok(Statement::Import {
                specifiers,
                source: src,
                assertions,
            })
        } else {
            Err(self.error_unexpected("Missing module source in import statement"))
        }
    }

    // Helper method to parse a module source string
    pub fn parse_module_source(&mut self) -> ParseResult<Option<Box<str>>> {
        if let TokenType::StringLiteral(s) = self.advance().unwrap().token_type.clone() {
            Ok(Some(s.into_boxed_str()))
        } else {
            Err(self.error_unexpected("Expected string literal for module source"))
        }
    }

    // Helper method to parse the 'from "module-name"' part
    pub fn parse_from_clause(&mut self) -> ParseResult<Option<Box<str>>> {
        self.consume(&TokenType::From, "Expected 'from' after import specifiers")?;
        self.parse_module_source()
    }

    // Helper method to parse namespace import: * as name
    pub fn parse_namespace_import(&mut self) -> ParseResult<ImportSpecifier> {
        self.consume(&TokenType::As, "Expected 'as' after '*'")?;
        // First advance to get the token
        let token = self.advance().unwrap().clone();
        // Then use the cloned token for identifier_name
        let local = self.identifier_name(&token)?;
        Ok(ImportSpecifier::Namespace(local))
    }

    // Helper method to parse default import: defaultExport
    pub fn parse_default_import(&mut self) -> ParseResult<ImportSpecifier> {
        let local = self.expect_identifier("Expected default import name")?;
        Ok(ImportSpecifier::Default(local))
    }

    // Helper method to parse named imports: { name1, name2 as alias2 }
    pub fn parse_named_imports(&mut self) -> ParseResult<Vec<ImportSpecifier>> {
        let mut specifiers = Vec::new();
        
        if !self.check(&TokenType::RightBrace) {
            loop {
                let imported = self.expect_identifier("Expected imported name")?;
                
                let local = if self.match_token(&TokenType::As) {
                    self.expect_identifier("Expected local name after 'as'")?
                } else {
                    imported.clone()
                };
                
                specifiers.push(ImportSpecifier::Named {
                    imported,
                    local,
                });
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after named imports")?;
        Ok(specifiers)
    }

    pub fn parse_import_assertions(&mut self) -> ParseResult<Vec<ImportAssertion>> {
        self.consume(&TokenType::LeftBrace, "Expected '{' after 'with'")?;
        
        let mut assertions = Vec::new();
        
        if !self.check(&TokenType::RightBrace) {
            loop {
                let key = self.expect_identifier("Expected assertion key")?;
                self.consume(&TokenType::Colon, "Expected ':' after assertion key")?;
                
                let value = if let TokenType::StringLiteral(s) = self.advance().unwrap().token_type.clone() {
                    s.into_boxed_str()
                } else {
                    return Err(self.error_unexpected("Expected string literal for assertion value"));
                };
                
                assertions.push(ImportAssertion { key, value });
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after import assertions")?;
        
        Ok(assertions)
    }

    pub fn parse_export_statement(&mut self) -> ParseResult<Statement> {
        let start_token = self.advance().unwrap().clone(); // consume 'export'
        
        // Handle export * from "module" or export * as name from "module"
        if self.match_token(&TokenType::Star) {
            return self.parse_export_all(&start_token);
        }
        
        // Handle export default ...
        if self.match_token(&TokenType::Default) {
            return self.parse_export_default(&start_token);
        }
        
        // Handle export declaration (var, let, const, function, class)
        if self.is_declaration_start() {
            return self.parse_export_declaration(&start_token);
        }
        
        // Handle export { ... } [from "..."]
        if self.match_token(&TokenType::LeftBrace) {
            return self.parse_export_named_specifiers(&start_token);
        }
        
        // If we get here, it's an invalid export statement
        Err(super::error::ParserError::new(
            "Invalid export statement. Expected '*', default, declaration, or named exports",
            start_token.line,
            start_token.column
        ))
    }

    // Helper method for export * from "module" or export * as name from "module"
    pub fn parse_export_all(&mut self, start_token: &Token) -> ParseResult<Statement> {
        let exported = if self.match_token(&TokenType::As) {
            Some(self.expect_identifier("Expected exported name after 'as'")?)
        } else {
            None
        };
        
        if !self.match_token(&TokenType::From) {
            return Err(super::error::ParserError::new(
                "Expected 'from' after export *",
                self.peek_token().unwrap().line,
                self.peek_token().unwrap().column
            ));
        }
        
        let source = self.parse_module_source()?
            .ok_or_else(|| super::error::ParserError::new(
                "Expected string literal for module source",
                self.previous().unwrap().line,
                self.previous().unwrap().column
            ))?;

        self.consume_semicolon("Expected ';' after export statement")?;
        
        Ok(Statement::Export(ExportDeclaration::All { source, exported }))
    }

    // Helper method for export default ...
    pub fn parse_export_default(&mut self, start_token: &Token) -> ParseResult<Statement> {
        let declaration = if self.check(&TokenType::Function) {
            let func_decl = self.parse_function_declaration()?;
            ExportDefaultDeclaration::Function(func_decl)
        } else if self.check(&TokenType::Class) {
            let class_decl = self.parse_class_declaration()?;
            ExportDefaultDeclaration::Class(class_decl)
        } else if self.check(&TokenType::Async) && self.is_async_function() {
            // Handle async function
            let func_decl = self.parse_async_function_declaration()?;
            ExportDefaultDeclaration::Function(func_decl)
        } else {
            // export default expression;
            let expr = self.parse_expression()?;
            self.consume_semicolon("Expected ';' after export default expression")?;
            ExportDefaultDeclaration::Expression(expr)
        };
        
        Ok(Statement::Export(ExportDeclaration::Default(Box::new(declaration))))
    }

    // Helper method for export declaration
    pub fn parse_export_declaration(&mut self, start_token: &Token) -> ParseResult<Statement> {
        let declaration = if self.check(&TokenType::Function) {
            Declaration::Function(self.parse_function_declaration()?)
        } else if self.check(&TokenType::Class) {
            Declaration::Class(self.parse_class_declaration()?)
        } else if self.check(&TokenType::Async) && self.is_async_function() {
            Declaration::Function(self.parse_async_function_declaration()?)
        } else if self.check(&TokenType::Var) || self.check(&TokenType::Let) || self.check(&TokenType::Const) {
            Declaration::Variable(self.parse_variable_declaration()?)
        } else {
            return Err(super::error::ParserError::new(
                "Expected declaration in export statement",
                self.peek_token().unwrap().line,
                self.peek_token().unwrap().column
            ));
        };
        
        Ok(Statement::Export(ExportDeclaration::Named {
            declaration: Some(Box::new(declaration)),
            specifiers: Vec::new(),
            source: None,
        }))
    }

    // Helper method for export { ... } [from "..."]
    pub fn parse_export_named_specifiers(&mut self, start_token: &Token) -> ParseResult<Statement> {
        let specifiers = self.parse_export_specifiers()?;
        
        // Optional from clause
        let source = if self.match_token(&TokenType::From) {
            Some(self.parse_module_source()?.ok_or_else(|| super::error::ParserError::new(
                "Expected string literal for module source",
                self.previous().unwrap().line,
                self.previous().unwrap().column
            ))?)
        } else {
            None
        };
        
        self.consume_semicolon("Expected ';' after export statement")?;
        
        Ok(Statement::Export(ExportDeclaration::Named {
            declaration: None,
            specifiers,
            source,
        }))
    }

    // Helper method to parse export specifiers: { name1, name2 as alias2 }
    pub fn parse_export_specifiers(&mut self) -> ParseResult<Vec<ExportSpecifier>> {
        let mut specifiers = Vec::new();
        
        if !self.check(&TokenType::RightBrace) {
            loop {
                let local = self.expect_identifier("Expected exported identifier")?;
                let exported = if self.match_token(&TokenType::As) {
                    self.expect_identifier("Expected exported name after 'as'")?
                } else {
                    local.clone()
                };
                
                specifiers.push(ExportSpecifier { local, exported });
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
                
                // Handle trailing comma
                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}' after export specifiers")?;
        Ok(specifiers)
    }

    // Helper method to check if the current token starts a declaration
    pub fn is_declaration_start(&self) -> bool {
        self.check(&TokenType::Var) || 
        self.check(&TokenType::Let) || 
        self.check(&TokenType::Const) ||
        self.check(&TokenType::Function) || 
        self.check(&TokenType::Class) ||
        (self.check(&TokenType::Async) && self.is_async_function())
    }
}
