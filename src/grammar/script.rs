use crate::ast::*;
use crate::parser::*;
use crate::unparser::*;
use super::statement::*;

pub struct ScriptNode;

impl ScriptNode {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Program> for ScriptNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Program> {        
        let mut body = Vec::new();
        while !parser.is_at_end() {
            body.push(StatementNode::new().parse(parser)?);
        }
        Ok(Program { source_type: SourceType::Script, body })
    }
}

impl UnparserCombinator<Program> for ScriptNode {
    fn unparse(&self, unparser: &mut Unparser, program: &Program) {
        for stmt in &program.body {
            StatementNode::new().unparse(unparser, stmt);
            unparser.newline();
        }
    }
}