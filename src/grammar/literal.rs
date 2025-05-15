use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::unparser::*;

pub struct LiteralNode;

impl LiteralNode {
    pub fn new() -> Self {
        Self
    }

    fn escape_string(&self, s: &str) -> String {
        let mut result = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                '\\' => result.push_str("\\\\"),
                '"' => result.push_str("\\\""),
                '\'' => result.push_str("\\'"),
                '\0' => result.push_str("\\0"),
                '\u{08}' => result.push_str("\\b"),  // backspace
                '\u{0C}' => result.push_str("\\f"),  // form feed
                c if c.is_control() => {
                    // Use Unicode escape sequence for other control characters
                    let code = c as u32;
                    result.push_str(&format!("\\u{:04x}", code));
                },
                _ => result.push(c),
            }
        }
        result
    }
}

impl ParserCombinator<Literal> for LiteralNode {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Literal> {        
        match parser.peek() {
            Token::StringLiteral(value) => {
                let value_clone = value.clone();
                parser.advance();
                return Ok(Literal::StringLiteral(StringLiteral { value: value_clone.into_boxed_str() }));
            },
            Token::NumberLiteral(value) => {
                let value_copy = *value;
                parser.advance();
                return Ok(Literal::NumericLiteral(NumericLiteral { value: value_copy }));
            },
            Token::BigIntLiteral(value) => {
                let value_clone = value.clone();
                parser.advance();
                return Ok(Literal::BigIntLiteral(BigIntLiteral { value: value_clone.into_boxed_str() }));
            },
            Token::RegExpLiteral(pattern, flags) => {
                let pattern_clone = pattern.clone();
                let flags_clone = flags.clone();
                parser.advance();
                return Ok(Literal::RegExpLiteral(RegExpLiteral { pattern: pattern_clone.into_boxed_str(), flags: flags_clone.into_boxed_str() }));
            },
            Token::True => {
                parser.advance();
                return Ok(Literal::BooleanLiteral(BooleanLiteral { value: true }));
            },
            Token::False => {
                parser.advance();
                return Ok(Literal::BooleanLiteral(BooleanLiteral { value: false }));
            },
            Token::Undefined => {
                parser.advance();
                return Ok(Literal::UndefinedLiteral(UndefinedLiteral {}));
            },
            Token::Null => {
                parser.advance();
                return Ok(Literal::NullLiteral(NullLiteral {}));
            },
            _ => return Err(parser.error_at_current("Expected a literal")),
        };
    }
}

impl UnparserCombinator<Expression> for LiteralNode {
    fn unparse(&self, unparser: &mut Unparser, expr: &Expression) {
        if let Expression::Literal(lit) = expr {
            match lit {
                Literal::StringLiteral(value) => {
                    let escaped = self.escape_string(&value.value);
                    unparser.write_char('"');
                    unparser.write_str(&escaped);
                    unparser.write_char('"');
                },
                Literal::NumericLiteral(value) => {
                    unparser.write_str(&value.value.to_string());
                },
                Literal::BooleanLiteral(value) => {
                    unparser.write_str(if value.value { "true" } else { "false" });
                },
                Literal::NullLiteral(_) => {
                    unparser.write_str("null");
                },
                Literal::UndefinedLiteral(_) => {
                    unparser.undefined();
                },
                Literal::RegExpLiteral(value) => {
                    unparser.write_char('/');
                    unparser.write_str(&value.pattern);
                    unparser.write_char('/');
                    unparser.write_str(&value.flags);
                },
                Literal::BigIntLiteral(value) => {
                    unparser.write_str(&value.value);
                    unparser.write_char('n');
                },
            }
        }
    }
}