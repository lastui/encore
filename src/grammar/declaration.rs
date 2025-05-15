use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::literal::*;
use super::pattern::*;
use super::expression::*;
use super::statement::*;
use super::class::*;

pub struct VariableDeclarationNode;

impl VariableDeclarationNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<VariableDeclaration> for VariableDeclarationNode {
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
        let declarator = VariableDeclaratorNode::new().parse(parser)?;
        
        // TODO not checking TDZ ?

        // For let/const, collect identifiers for TDZ enforcement
        if matches!(kind, VariableKind::Let | VariableKind::Const) {
            collect_binding_identifiers(&declarator.id, &mut declared_identifiers);
        }
        
        declarations.push(declarator);
        
        // Parse additional declarators
        while parser.consume(&Token::Comma) {
            let declarator = VariableDeclaratorNode::new().parse(parser)?;
            
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

impl UnparserCombinator<VariableDeclaration> for VariableDeclarationNode {
    fn unparse(&self, unparser: &mut Unparser, node: &VariableDeclaration) {
        // Write the variable kind (var, let, const)
        match node.kind {
            VariableKind::Var => unparser.write_str("var"),
            VariableKind::Let => unparser.write_str("let"),
            VariableKind::Const => unparser.write_str("const"),
        }

        unparser.write_char(' ');
        
        // Write the declarations
        if !node.declarations.is_empty() {
            // First declaration
            VariableDeclaratorNode::new().unparse(unparser, &node.declarations[0]);
            
            // Remaining declarations
            for decl in &node.declarations[1..] {
                unparser.write_char(',');
                unparser.space();
                VariableDeclaratorNode::new().unparse(unparser, decl);
            }
        }
        
        // Add semicolon
        unparser.write_char(';');
    }
}


/// Parser for variable declarators
pub struct VariableDeclaratorNode;

impl VariableDeclaratorNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<VariableDeclarator> for VariableDeclaratorNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<VariableDeclarator> {
        // Parse the identifier or pattern
        let id = PatternNode::new().parse(parser)?;
        
        // Parse the initializer if present
        let init = if parser.consume(&Token::Equal) {
            Some(Box::new(ExpressionNode::new().parse(parser)?))
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
        // Check if this is an async function
        let async_function = parser.consume(&Token::Async);
        
        // Consume the 'function' keyword
        parser.assert_consume(&Token::Function, "Expected 'function'")?;
        
        // Check if this is a generator function
        let generator = parser.consume(&Token::Star);
        
        // Parse the function name
        let id = if matches!(parser.peek(), Token::Identifier(_)) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };
            
        // Parse the parameter list
        parser.assert_consume(&Token::LeftParen, "Expected '(' after function name")?;
        
        let mut params = Vec::new();
        
        if !parser.check(&Token::RightParen) {
            // Parse the first parameter
            params.push(PatternNode::new().parse(parser)?);
            
            // Parse additional parameters
            while parser.consume(&Token::Comma) {
                params.push(PatternNode::new().parse(parser)?);
            }
        }
        
        parser.assert_consume(&Token::RightParen, "Expected ')' after function parameters")?;
        
        let body = parser.with_context(LexicalContext::FunctionBody { allow_await: async_function, allow_yield: generator }, |p| {
            BlockStatementNode::new().parse(p)
        })?;

        Ok(FunctionDeclaration {
            id,
            params,
            body,
            generator,
            async_function,
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

// Add these new parser structs for each export type
pub struct ExportNamedDeclarationParser;
pub struct ExportDefaultDeclarationParser;
pub struct ExportAllDeclarationParser;

impl ExportNamedDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ExportDefaultDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

impl ExportAllDeclarationParser {
    pub fn new() -> Self {
        Self
    }
}

// Implement parsing for each export type
impl ParserCombinator<ExportNamedDeclaration> for ExportNamedDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExportNamedDeclaration> {
        // Consume the 'export' keyword
        parser.assert_consume(&Token::Export, "Expected 'export'")?;
        
        // Parse declaration if present
        let declaration = if parser.check(&Token::Var) || parser.check(&Token::Let) || parser.check(&Token::Const) {
            // Variable declaration
            let var_decl = VariableDeclarationNode::new().parse(parser)?;
            Some(Box::new(Declaration::VariableDeclaration(var_decl)))
        } else if parser.check(&Token::Function) {
            // Function declaration
            let func_decl = FunctionDeclarationParser::new().parse(parser)?;
            Some(Box::new(Declaration::FunctionDeclaration(func_decl)))
        } else if parser.check(&Token::Class) {
            // Class declaration
            let class_decl = ClassDeclarationNode::new().parse(parser)?;
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
                    let literal = LiteralNode::new().parse(parser)?;
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

        Ok(ExportNamedDeclaration {
            declaration,
            specifiers,
            source,
        })
    }
}

impl ParserCombinator<ExportDefaultDeclaration> for ExportDefaultDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExportDefaultDeclaration> {
        // Consume the 'export' and 'default' keywords
        parser.assert_consume(&Token::Export, "Expected 'export'")?;
        parser.assert_consume(&Token::Default, "Expected 'default'")?;
        
        // Parse the declaration or expression
        let declaration = if parser.check(&Token::Function) || parser.check(&Token::Class) {
            // Function or class declaration
            if parser.check(&Token::Function) {
                let func = FunctionDeclarationParser::new().parse(parser)?;
                ExportDefaultDeclarationKind::Declaration(Box::new(Declaration::FunctionDeclaration(func)))
            } else {
                let class = ClassDeclarationNode::new().parse(parser)?;
                ExportDefaultDeclarationKind::Declaration(Box::new(Declaration::ClassDeclaration(class)))
            }
        } else {
            // Expression
            let expr = ExpressionNode::new().parse(parser)?;
            parser.consume_semicolon("Expected ';' after export default expression")?;
            ExportDefaultDeclarationKind::Expression(Box::new(expr))
        };

        Ok(ExportDefaultDeclaration { declaration })
    }
}

impl ParserCombinator<ExportAllDeclaration> for ExportAllDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExportAllDeclaration> {
        // Consume the 'export' and '*' tokens
        parser.assert_consume(&Token::Export, "Expected 'export'")?;
        parser.assert_consume(&Token::Star, "Expected '*'")?;
        
        // Parse 'as' clause if present
        let exported = if parser.consume(&Token::As) {
            Some(IdentifierNode::new().parse(parser)?)
        } else {
            None
        };
        
        // Parse 'from' clause
        parser.assert_consume(&Token::From, "Expected 'from' after export *")?;
        
        // Parse the module source
        let source = match parser.peek() {
            Token::StringLiteral(_) => {
                let literal = LiteralNode::new().parse(parser)?;
                match literal {
                    Literal::StringLiteral(str_lit) => str_lit,
                    _ => return Err(parser.error_at_current("Expected string literal for module source")),
                }
            },
            _ => return Err(parser.error_at_current("Expected string literal for module source")),
        };
        
        // Consume the semicolon
        parser.consume_semicolon("Expected ';' after export * from declaration")?;

        Ok(ExportAllDeclaration {
            source,
            exported,
        })
    }
}

// Update the ExportDeclarationParser to use the new specific parsers
impl ParserCombinator<ExportDeclaration> for ExportDeclarationParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<ExportDeclaration> {
        // Save position to check what type of export this is
        let pos = parser.save_position();
        
        // Consume the 'export' keyword
        parser.assert_consume(&Token::Export, "Expected 'export'")?;
        
        // Check for export type
        if parser.check(&Token::Default) {
            // Restore position and parse as default export
            parser.restore_position(pos);
            ExportDefaultDeclarationParser::new().parse(parser).map(ExportDeclaration::Default)
        } else if parser.check(&Token::Star) {
            // Restore position and parse as export all
            parser.restore_position(pos);
            ExportAllDeclarationParser::new().parse(parser).map(ExportDeclaration::All)
        } else {
            // Restore position and parse as named export
            parser.restore_position(pos);
            ExportNamedDeclarationParser::new().parse(parser).map(ExportDeclaration::Named)
        }
    }
}

// Helper method for ExportNamedDeclarationParser
impl ExportNamedDeclarationParser {
    fn parse_export_specifier(&self, parser: &mut Parser) -> ParseResult<ExportSpecifier> {
        // Parse the local name
        let local = IdentifierNode::new().parse(parser)?;
        
        // Parse the exported name if present
        let exported = if parser.consume(&Token::As) {
            IdentifierNode::new().parse(parser)?
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
            let local = IdentifierNode::new().parse(parser)?;
            
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
            let local = IdentifierNode::new().parse(parser)?;
            
            specifiers.push(ImportSpecifier::ImportDefaultSpecifier(ImportDefaultSpecifier {
                local,
            }));
            
            // Check for additional named imports
            if parser.consume(&Token::Comma) {
                if parser.consume(&Token::Star) {
                    // Namespace import after default import
                    parser.assert_consume(&Token::As, "Expected 'as' after '*' in import declaration")?;
                    let local = IdentifierNode::new().parse(parser)?;
                    
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
                let literal = LiteralNode::new().parse(parser)?;
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
        let imported = IdentifierNode::new().parse(parser)?;
        
        // Parse the local name if present
        let local = if parser.consume(&Token::As) {
            IdentifierNode::new().parse(parser)?
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

// Variable declarator unparser
impl UnparserCombinator<VariableDeclarator> for VariableDeclaratorNode {
    fn unparse(&self, unparser: &mut Unparser, node: &VariableDeclarator) {
        // Write the identifier or pattern
        PatternNode::new().unparse(unparser, &node.id);
        
        // Write the initializer if present
        if let Some(init) = &node.init {
            unparser.space();
            unparser.write_char('=');
            unparser.space();
            ExpressionNode::new().unparse(unparser, init);
        }
    }
}

// Function declaration unparser
impl UnparserCombinator<FunctionDeclaration> for FunctionDeclarationParser {
    fn unparse(&self, unparser: &mut Unparser, node: &FunctionDeclaration) {
        // Write async if it's an async function
        if node.async_function {
            unparser.write_str("async");
            unparser.write_char(' ');
        }
        
        // Write the function keyword
        unparser.write_str("function");
        
        // Write * if it's a generator function
        if node.generator {
            unparser.write_char('*');
        }
        
        // Write the function name if present
        if let Some(id) = &node.id {
            unparser.write_char(' ');
            unparser.write_str(&id.name);
        }
        
        // Write the parameter list
        unparser.write_char('(');
        
        // Write parameters
        if !node.params.is_empty() {
            PatternNode::new().unparse(unparser, &node.params[0]);
            
            for param in &node.params[1..] {
                unparser.write_char(',');
                unparser.space();
                PatternNode::new().unparse(unparser, param);
            }
        }
        
        unparser.write_char(')');
        unparser.space();
        
        // Write the function body
        BlockStatementNode::new().unparse(unparser, &node.body);
    }
}

// Export named declaration unparser
impl UnparserCombinator<ExportNamedDeclaration> for ExportNamedDeclarationParser {
    fn unparse(&self, unparser: &mut Unparser, node: &ExportNamedDeclaration) {
        unparser.write_str("export");
        unparser.write_char(' ');
        
        // Handle declaration if present
        if let Some(decl) = &node.declaration {
            match &**decl {
                Declaration::VariableDeclaration(var_decl) => {
                    VariableDeclarationNode::new().unparse(unparser, var_decl);
                },
                Declaration::FunctionDeclaration(func_decl) => {
                    FunctionDeclarationParser::new().unparse(unparser, func_decl);
                },
                Declaration::ClassDeclaration(class_decl) => {
                    // Assuming ClassDeclarationNode is implemented elsewhere
                    ClassDeclarationNode::new().unparse(unparser, class_decl);
                },
                _ => {
                    // This shouldn't happen for export named declarations
                    unparser.write_str("/* unsupported declaration */");
                }
            }
        } else {
            // Export specifiers
            unparser.write_char('{');
            
            if !node.specifiers.is_empty() {
                unparser.space();
                
                // First specifier
                self.unparse_export_specifier(unparser, &node.specifiers[0]);
                
                // Remaining specifiers
                for spec in &node.specifiers[1..] {
                    unparser.write_char(',');
                    unparser.space();
                    self.unparse_export_specifier(unparser, spec);
                }
                
                unparser.space();
            }
            
            unparser.write_char('}');
            
            // Handle 'from' clause if present
            if let Some(source) = &node.source {
                unparser.write_char(' ');
                unparser.write_str("from");
                unparser.write_char(' ');
                unparser.write_str(&format!("\"{}\"", source.value));
            }
            
            unparser.write_char(';');
        }
    }
}

// Export default declaration unparser
impl UnparserCombinator<ExportDefaultDeclaration> for ExportDefaultDeclarationParser {
    fn unparse(&self, unparser: &mut Unparser, node: &ExportDefaultDeclaration) {
        unparser.write_str("export");
        unparser.write_char(' ');
        unparser.write_str("default");
        unparser.write_char(' ');
        
        match &node.declaration {
            ExportDefaultDeclarationKind::Declaration(decl) => {
                match &**decl {
                    Declaration::FunctionDeclaration(func_decl) => {
                        FunctionDeclarationParser::new().unparse(unparser, func_decl);
                    },
                    Declaration::ClassDeclaration(class_decl) => {
                        ClassDeclarationNode::new().unparse(unparser, class_decl);
                    },
                    _ => {
                        // This shouldn't happen for export default declarations
                        unparser.write_str("/* unsupported declaration */");
                    }
                }
            },
            ExportDefaultDeclarationKind::Expression(expr) => {
                ExpressionNode::new().unparse(unparser, expr);
                unparser.write_char(';');
            }
        }
    }
}

// Export all declaration unparser
impl UnparserCombinator<ExportAllDeclaration> for ExportAllDeclarationParser {
    fn unparse(&self, unparser: &mut Unparser, node: &ExportAllDeclaration) {
        unparser.write_str("export");
        unparser.write_char(' ');
        unparser.write_char('*');
        
        // Handle 'as' clause if present
        if let Some(exported) = &node.exported {
            unparser.write_char(' ');
            unparser.write_str("as");
            unparser.write_char(' ');
            unparser.write_str(&exported.name);
        }
        
        unparser.write_char(' ');
        unparser.write_str("from");
        unparser.write_char(' ');
        unparser.write_str(&format!("\"{}\"", node.source.value));
        unparser.write_char(';');
    }
}

// Import declaration unparser
impl UnparserCombinator<ImportDeclaration> for ImportDeclarationParser {
    fn unparse(&self, unparser: &mut Unparser, node: &ImportDeclaration) {
        unparser.write_str("import");
        unparser.write_char(' ');
        
        // Handle specifiers
        if node.specifiers.is_empty() {
            // Side-effect import (no specifiers)
        } else {
            let mut has_default = false;
            let mut has_namespace = false;
            let mut named_specifiers = Vec::new();
            
            // Categorize specifiers
            for spec in &node.specifiers {
                match spec {
                    ImportSpecifier::ImportDefaultSpecifier(default_spec) => {
                        has_default = true;
                        unparser.write_str(&default_spec.local.name);
                    },
                    ImportSpecifier::ImportNamespaceSpecifier(namespace_spec) => {
                        has_namespace = true;
                        if has_default {
                            unparser.write_char(',');
                            unparser.space();
                        }
                        unparser.write_str("* as ");
                        unparser.write_str(&namespace_spec.local.name);
                    },
                    ImportSpecifier::ImportSpecifier(named_spec) => {
                        named_specifiers.push(named_spec);
                    }
                }
            }
            
            // Handle named specifiers
            if !named_specifiers.is_empty() {
                if has_default || has_namespace {
                    unparser.write_char(',');
                    unparser.space();
                }
                
                unparser.write_char('{');
                unparser.space();
                
                // First named specifier
                self.unparse_import_specifier(unparser, &named_specifiers[0]);
                
                // Remaining named specifiers
                for spec in &named_specifiers[1..] {
                    unparser.write_char(',');
                    unparser.space();
                    self.unparse_import_specifier(unparser, spec);
                }
                
                unparser.space();
                unparser.write_char('}');
            }
            
            //unparser.space();
            unparser.write_char(' ');
        }
        
        // Write the source
        unparser.write_str("from");
        unparser.write_char(' ');
        unparser.write_str(&format!("\"{}\"", node.source.value));
        unparser.write_char(';');
    }
}

// Helper methods for ExportNamedDeclarationParser
impl ExportNamedDeclarationParser {
    fn unparse_export_specifier(&self, unparser: &mut Unparser, spec: &ExportSpecifier) {
        unparser.write_str(&spec.local.name);
        
        // If the exported name is different from the local name
        if spec.local.name != spec.exported.name {
            unparser.write_char(' ');
            unparser.write_str("as");
            unparser.write_char(' ');
            unparser.write_str(&spec.exported.name);
        }
    }
}

// Helper methods for ImportDeclarationParser
impl ImportDeclarationParser {
    fn unparse_import_specifier(&self, unparser: &mut Unparser, spec: &NamedImportSpecifier) {
        unparser.write_str(&spec.imported.name);
        
        // If the local name is different from the imported name
        if spec.imported.name != spec.local.name {
            unparser.write_char(' ');
            unparser.write_str("as");
            unparser.write_char(' ');
            unparser.write_str(&spec.local.name);
        }
    }
}
