use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::literal::*;
use super::pattern::*;
use super::expression::*;
use super::statement::*;
use super::class::*;

pub struct VariableDeclarationParser;

impl VariableDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<VariableDeclaration> for VariableDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<VariableDeclaration> {
        // Parse variable kind
        let kind = if parser.consume(&Token::Var) {
            VariableKind::Var
        } else if parser.consume(&Token::Let) {
            VariableKind::Let
        } else if parser.consume(&Token::Const) {
            VariableKind::Const
        } else {
            return Err(parser.error_at_current("Expected variable declaration"));
        };
        
        let mut declarations = Vec::new();
        
        // For let/const, track declared identifiers to enforce TDZ
        let mut declared_identifiers = Vec::new();
        
        // Parse first declarator
        let declarator = VariableDeclaratorParser::new().parse(parser)?;
        
        // For let/const, collect identifiers for TDZ enforcement
        if matches!(kind, VariableKind::Let | VariableKind::Const) {
            collect_binding_identifiers(&declarator.id, &mut declared_identifiers);
        }
        
        declarations.push(declarator);
        
        // Parse additional declarators
        while parser.consume(&Token::Comma) {
            let declarator = VariableDeclaratorParser::new().parse(parser)?;
            
            // For let/const, collect identifiers and check TDZ
            if matches!(kind, VariableKind::Let | VariableKind::Const) {
                // If there's an initializer, check that it doesn't reference any of the declared identifiers
                if let Some(ref init) = declarator.init {
                    check_tdz_violation(init, &declared_identifiers, parser)?;
                }
                
                // Add new identifiers to the list
                collect_binding_identifiers(&declarator.id, &mut declared_identifiers);
            }
            
            declarations.push(declarator);
        }
        
        Ok(VariableDeclaration {
            declarations,
            kind,
        })
    }
}

/// Parser for variable declarators
pub struct VariableDeclaratorParser;

impl VariableDeclaratorParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<VariableDeclarator> for VariableDeclaratorParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<VariableDeclarator> {
        // Parse the identifier or pattern
        let id = PatternParser::new().parse(parser)?;
        
        // Parse the initializer if present
        let init = if parser.consume(&Token::Equal) {
            Some(Box::new(ExpressionParser::new().parse(parser)?))
        } else {
            None
        };
        
        Ok(VariableDeclarator {
            id,
            init,
        })
    }
}

/// Parser for function declarations
pub struct FunctionDeclarationParser;

impl FunctionDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<FunctionDeclaration> for FunctionDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<FunctionDeclaration> {
        // Consume the 'function' keyword
        parser.assert_consume(&Token::Function, "Expected 'function'")?;
        
        // Check if this is a generator function
        let generator = parser.consume(&Token::Star);
        
        // Parse the function name
        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierParser::new().parse(parser)?)
        } else {
            None
        };
            
        // Parse the parameter list
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !parser.check(&Token::RightParen) {
            // Parse the first parameter
            params.push(PatternParser::new().parse(parser)?);
            
            // Parse additional parameters
            while parser.consume(&Token::Comma) {
                params.push(PatternParser::new().parse(parser)?);
            }
        }
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after function parameters")?;
        
        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: false, allow_yield: generator }, |p| {
            BlockStatementParser::new().parse(p)
        })?;

        Ok(FunctionDeclaration {
            id,
            params,
            body,
            generator,
            async_function: false,
        })
    }
}

/// Enum for export declarations
pub enum ExportDeclaration {
    Named(ExportNamedDeclaration),
    Default(ExportDefaultDeclaration),
    All(ExportAllDeclaration),
}

/// Parser for export declarations
pub struct ExportDeclarationParser;

impl ExportDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ExportDeclaration> for ExportDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExportDeclaration> {
        // Consume the 'export' keyword
        parser.assert_consume(&Token::Export, "Expected 'export'")?;
        
        // Check for export type
        if parser.consume(&Token::Default) {
            // Export default declaration
            let declaration = if parser.check(&Token::Function) || parser.check(&Token::Class) {
                // Function or class declaration
                if parser.check(&Token::Function) {
                    let func = FunctionDeclarationParser::new().parse(parser)?;
                    ExportDefaultDeclarationKind::Declaration(Box::new(Declaration::FunctionDeclaration(func)))
                } else {
                    let class = ClassDeclarationParser::new().parse(parser)?;
                    ExportDefaultDeclarationKind::Declaration(Box::new(Declaration::ClassDeclaration(class)))
                }
            } else {
                // Expression
                let expr = ExpressionParser::new().parse(parser)?;
                parser.consume_semicolon("Expected ';' after export default expression")?;
                ExportDefaultDeclarationKind::Expression(Box::new(expr))
            };

            Ok(ExportDeclaration::Default(ExportDefaultDeclaration { declaration }))
        } else if parser.consume(&Token::Star) {
            // Export all declaration
            let exported = if parser.consume(&Token::As) {
                Some(IdentifierParser::new().parse(parser)?)
            } else {
                None
            };
            
            parser.assert_consume(&Token::From, "Expected 'from' after export *")?;
            
            let source = match parser.peek() {
                Token::StringLiteral(_) => {
                    let literal = LiteralParser::new().parse(parser)?;
                    match literal {
                        Literal::StringLiteral(str_lit) => str_lit,
                        _ => return Err(parser.error_at_current("Expected string literal for module source")),
                    }
                },
                _ => return Err(parser.error_at_current("Expected string literal for module source")),
            };
            
            parser.consume_semicolon("Expected ';' after export * from declaration")?;

            Ok(ExportDeclaration::All(ExportAllDeclaration {
                source,
                exported,
            }))
        } else {
            // Named export declaration
            let declaration = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
                // Variable declaration
                let var_decl = VariableDeclarationParser::new().parse(parser)?;
                Some(Box::new(Declaration::VariableDeclaration(var_decl)))
            } else if parser.check(&Token::Function) {
                // Function declaration
                let func_decl = FunctionDeclarationParser::new().parse(parser)?;
                Some(Box::new(Declaration::FunctionDeclaration(func_decl)))
            } else if parser.check(&Token::Class) {
                // Class declaration
                let class_decl = ClassDeclarationParser::new().parse(parser)?;
                Some(Box::new(Declaration::ClassDeclaration(class_decl)))
            } else {
                None
            };
            
            // If there's no declaration, there must be export specifiers
            let mut specifiers = Vec::new();
            
            if declaration.is_none() {
                // Parse export specifiers
                parser.assert_consume(&Token::LeftBrace, "Expected '{' in named export declaration")?;
                
                if !parser.check(&Token::RightBrace) {
                    // Parse the first specifier
                    specifiers.push(self.parse_export_specifier(parser)?);
                    
                    // Parse additional specifiers
                    while parser.consume(&Token::Comma) && !parser.check(&Token::RightBrace) {
                        specifiers.push(self.parse_export_specifier(parser)?);
                    }
                }
                
                parser.assert_consume(&Token::RightBrace, "Expected '}' after export specifiers")?;
            }
            
            // Parse the 'from' clause if present
            let source = if parser.consume(&Token::From) {
                match parser.peek() {
                    Token::StringLiteral(_) => {
                        let literal = LiteralParser::new().parse(parser)?;
                        match literal {
                            Literal::StringLiteral(str_lit) => Some(str_lit),
                            _ => return Err(parser.error_at_current("Expected string literal for module source")),
                        }
                    },
                    _ => return Err(parser.error_at_current("Expected string literal for module source")),
                }
            } else {
                None
            };
            
            // Consume the semicolon
            parser.consume_semicolon("Expected ';' after export declaration")?;

            Ok(ExportDeclaration::Named(ExportNamedDeclaration {
                declaration,
                specifiers,
                source,
            }))
        }
    }
}

impl ExportDeclarationParser {
    fn parse_export_specifier(&self, parser: &mut Parser) -> ParseResult<ExportSpecifier> {
        // Parse the local name
        let local = IdentifierParser::new().parse(parser)?;
        
        // Parse the exported name if present
        let exported = if parser.consume(&Token::As) {
            IdentifierParser::new().parse(parser)?
        } else {
            // If no 'as', the exported name is the same as the local name
            Identifier { name: local.name.clone() }
        };
        
        Ok(ExportSpecifier {
            local,
            exported,
        })
    }
}

/// Parser for import declarations
pub struct ImportDeclarationParser;

impl ImportDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<ImportDeclaration> for ImportDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ImportDeclaration> {        
        // Consume the 'import' keyword
        parser.assert_consume(&Token::Import, "Expected 'import'")?;
        
        let mut specifiers = Vec::new();
        
        // Check for import type
        if matches!(parser.peek(), Token::StringLiteral(_)) {
            // Import without specifiers (side-effect import)
            // No specifiers to add
        } else if parser.consume(&Token::Star) {
            // Namespace import
            parser.assert_consume(&Token::As, "Expected 'as' after '*' in import declaration")?;
            let local = IdentifierParser::new().parse(parser)?;
            
            specifiers.push(ImportSpecifier::ImportNamespaceSpecifier(ImportNamespaceSpecifier {
                local,
            }));
            
            parser.assert_consume(&Token::From, "Expected 'from' after namespace import")?;
        } else if parser.check(&Token::LeftBrace) {
            // Named imports
            parser.assert_consume(&Token::LeftBrace, "Expected '{' in named import")?;
            
            if !parser.check(&Token::RightBrace) {
                // Parse the first specifier
                specifiers.push(self.parse_import_specifier(parser)?);
                
                // Parse additional specifiers
                while parser.consume(&Token::Comma) && !parser.check(&Token::RightBrace) {
                    specifiers.push(self.parse_import_specifier(parser)?);
                }
            }
            
            parser.assert_consume(&Token::RightBrace, "Expected '}' after import specifiers")?;
            
            parser.assert_consume(&Token::From, "Expected 'from' after named imports")?;
        } else if matches!(parser.peek(), Token::Identifier(_)) {
            // Default import
            let local = IdentifierParser::new().parse(parser)?;
            
            specifiers.push(ImportSpecifier::ImportDefaultSpecifier(ImportDefaultSpecifier {
                local,
            }));
            
            // Check for additional named imports
            if parser.consume(&Token::Comma) {
                if parser.consume(&Token::Star) {
                    // Namespace import after default import
                    parser.assert_consume(&Token::As, "Expected 'as' after '*' in import declaration")?;
                    let local = IdentifierParser::new().parse(parser)?;
                    
                    specifiers.push(ImportSpecifier::ImportNamespaceSpecifier(ImportNamespaceSpecifier {
                        local,
                    }));
                } else if parser.consume(&Token::LeftBrace) {
                    // Named imports after default import
                    if !parser.check(&Token::RightBrace) {
                        // Parse the first specifier
                        specifiers.push(self.parse_import_specifier(parser)?);
                        
                        // Parse additional specifiers
                        while parser.consume(&Token::Comma) && !parser.check(&Token::RightBrace) {
                            specifiers.push(self.parse_import_specifier(parser)?);
                        }
                    }
                    
                    parser.assert_consume(&Token::RightBrace, "Expected '}' after import specifiers")?;
                }
            }
            
            parser.assert_consume(&Token::From, "Expected 'from' after import specifiers")?;
        }
        
        // Parse the source
        let source = match parser.peek() {
            Token::StringLiteral(_) => {
                let literal = LiteralParser::new().parse(parser)?;
                match literal {
                    Literal::StringLiteral(str_lit) => str_lit,
                    _ => return Err(parser.error_at_current("Expected string literal for module source")),
                }
            },
            _ => return Err(parser.error_at_current("Expected string literal for module source")),
        };
        
        // Consume the semicolon
        parser.consume_semicolon("Expected ';' after import declaration")?;
        
        Ok(ImportDeclaration {
            specifiers,
            source,
        })
    }
}

impl ImportDeclarationParser {
    fn parse_import_specifier(&self, parser: &mut Parser) -> ParseResult<ImportSpecifier> {
        // Parse the imported name
        let imported = IdentifierParser::new().parse(parser)?;
        
        // Parse the local name if present
        let local = if parser.consume(&Token::As) {
            IdentifierParser::new().parse(parser)?
        } else {
            // If no 'as', the local name is the same as the imported name
            Identifier { name: imported.name.clone() }
        };
        
        Ok(ImportSpecifier::ImportSpecifier(NamedImportSpecifier {
            imported,
            local,
        }))
    }
}




// Helper function to collect identifiers from a binding pattern
fn collect_binding_identifiers(pattern: &Pattern, identifiers: &mut Vec<Box<str>>) {
    match pattern {
        Pattern::Identifier(ident) => {
            identifiers.push(ident.name.clone());
        },
        Pattern::ObjectPattern(obj_pattern) => {
            for prop in &obj_pattern.properties {
                match prop {
                    ObjectPatternProperty::Property(prop) => {
                        collect_binding_identifiers(&prop.value, identifiers);
                    },
                    ObjectPatternProperty::RestElement(rest) => {
                        collect_binding_identifiers(&rest.argument, identifiers);
                    },
                }
            }
        },
        Pattern::ArrayPattern(arr_pattern) => {
            for elem in arr_pattern.elements.iter().flatten() {
                collect_binding_identifiers(elem, identifiers);
            }
        },
        Pattern::RestElement(rest) => {
            collect_binding_identifiers(&rest.argument, identifiers);
        },
        Pattern::AssignmentPattern(assign) => {
            collect_binding_identifiers(&assign.left, identifiers);
        },
        _ => {}
    }
}

// Helper function to check for TDZ violations in initializers
fn check_tdz_violation(expr: &Expression, declared_identifiers: &[Box<str>], parser: &mut Parser) -> ParseResult<()> {
    match expr {
        Expression::Identifier(ident) => {
            if declared_identifiers.contains(&ident.name) {
                return Err(parser.error_at_current(&format!(
                    "Cannot access '{}' before initialization (temporal dead zone violation)",
                    ident.name
                )));
            }
        },
        Expression::MemberExpression(member) => {
            check_tdz_violation(&member.object, declared_identifiers, parser)?;
            if let MemberProperty::Expression(ref expr) = member.property {
                check_tdz_violation(expr, declared_identifiers, parser)?;
            }
        },
        Expression::CallExpression(call) => {
            check_tdz_violation(&call.callee, declared_identifiers, parser)?;
            for arg in &call.arguments {
                check_tdz_violation(arg, declared_identifiers, parser)?;
            }
        },
        // Add checks for other expression types as needed
        _ => {}
    }
    Ok(())
}