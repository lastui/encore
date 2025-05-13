use super::error::ParseResult;
use super::parser::Parser;

pub trait ParserCombinator<T> {
    fn parse(&self, parser: &mut Parser) -> ParseResult<T>;
}
