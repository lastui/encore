use crate::ast::*;
use super::formatter::{Formatter, FormatStyle};
use super::combinator::UnparserCombinator;

use crate::grammar::*;

pub struct Unparser {
    formatter: Formatter,
}

impl Unparser {
    pub fn new(style: FormatStyle) -> Self {
        Self {
            formatter: Formatter::new(style),
        }
    }

    // Main unparse methods
    pub fn unparse_module(&mut self, program: &Program) -> &str {
        // TODO generic
        ModuleNode::new().unparse(self, program);
        self.formatter.as_str()
    }
        
    pub fn unparse_script(&mut self, program: &Program) -> &str {
        ScriptNode::new().unparse(self, program);
        self.formatter.as_str()
    }

    /*
    pub fn unparse_expression(&mut self, expr: &Expression) -> String {
        ExpressionNode::new().unparse(self, expr);
        self.formatter.into_string()
    }
    
    pub fn unparse_statement(&mut self, stmt: &Statement) -> String {
        StatementNode::new().unparse(self, stmt);
        self.formatter.into_string()
    }
    */

    // Formatter delegations
    pub fn write_str(&mut self, s: &str) {
        self.formatter.write_str(s);
    }

    pub fn write_char(&mut self, c: char) {
        self.formatter.write_char(c);
    }

    pub fn newline(&mut self) {
        self.formatter.newline();
    }

    pub fn undefined(&mut self) {
        self.formatter.undefined();
    }

    pub fn space(&mut self) {
        self.formatter.space();
    }

    pub fn indent(&mut self) {
        self.formatter.indent();
    }

    pub fn dedent(&mut self) {
        self.formatter.dedent();
    }

    pub fn with_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self)
    {
        self.indent();
        f(self);
        self.dedent();
    }
}
