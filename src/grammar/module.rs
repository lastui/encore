use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;
use super::statement::*;

pub struct ModuleNode;

impl ModuleNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Program> for ModuleNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Program> {
        let body = parser.with_context(LexicalContext::ModuleBody { allow_await: true }, |p| {
            let mut result = Vec::new();
            while !p.is_at_end() {
                result.push(StatementNode::new().parse(p)?);
            }
            Ok(result)
        })?;
        Ok(Program { source_type: SourceType::Module, body })
    }
}

impl UnparserCombinator<Program> for ModuleNode {
    fn unparse(&self, unparser: &mut Unparser, program: &Program) {
        for stmt in &program.body {
            StatementNode::new().unparse(unparser, stmt);
            unparser.newline();
        }
    }
}