use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use super::statement::*;

pub struct ModuleParser;

impl ModuleParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Program> for ModuleParser {
    
    fn parse(&self, parser: &mut Parser) -> ParseResult<Program> {

        let body = parser.with_context(LexicalContext::ModuleBody { allow_await: true }, |p| {
            let mut result = Vec::new();
            while !p.is_at_end() {
                let statement = StatementParser::new().parse(p)?;
                result.push(statement);
            }
            Ok(result)
        })?;

        Ok(Program { source_type: SourceType::Module, body })
    }
}

pub struct ScriptParser;

impl ScriptParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Program> for ScriptParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Program> {        
        let mut body = Vec::new();

        while !parser.is_at_end() {
            let statement = StatementParser::new().parse(parser)?;
            body.push(statement);
        }

        Ok(Program { source_type: SourceType::Script, body })
    }
}

