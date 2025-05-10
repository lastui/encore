use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;

pub struct LiteralParser;

impl LiteralParser {
    pub fn new() -> Self {
        Self
    }
}

impl ParserCombinator<Literal> for LiteralParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Literal> {        
        match parser.peek() {
            Token::StringLiteral(value) => {
                let value_clone = value.clone();
                parser.advance();

                return Ok(Literal::StringLiteral(StringLiteral {
                    value: value_clone.into_boxed_str(),
                }));
            },
            Token::NumberLiteral(value) => {
                let value_copy = *value;
                parser.advance();
                return Ok(Literal::NumericLiteral(NumericLiteral {
                    value: value_copy,
                }));
            },
            Token::BigIntLiteral(value) => {
                let value_clone = value.clone();
                parser.advance();
                return Ok(Literal::BigIntLiteral(BigIntLiteral {
                    value: value_clone.into_boxed_str(),
                }));
            },
            Token::RegExpLiteral(pattern, flags) => {
                let pattern_clone = pattern.clone();
                let flags_clone = flags.clone();
                parser.advance();
                return Ok(Literal::RegExpLiteral(RegExpLiteral {
                    pattern: pattern_clone.into_boxed_str(),
                    flags: flags_clone.into_boxed_str(),
                }));
            },
            Token::True => {
                parser.advance();
                return Ok(Literal::BooleanLiteral(BooleanLiteral {
                    value: true,
                }));
            },
            Token::False => {
                parser.advance();
                return Ok(Literal::BooleanLiteral(BooleanLiteral {
                    value: false,
                }));
            },
            Token::Null => {
                parser.advance();
                return Ok(Literal::NullLiteral(NullLiteral {}));
            },
            _ => return Err(parser.error_at_current("Expected a literal")),
        };
    }
}
