use crate::lexer::Token;

pub struct TokenStream<'a> {
    tokens: &'a [(Token, [usize; 2])],
    current: usize,
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [(Token, [usize; 2])]) -> Self {
        Self {
            tokens,
            current: 0,
            source: "",
        }
    }

    pub fn attach_source(&mut self, source: &'a str) {
        self.source = source;
    }

    pub fn get_source_text(&self) -> &str {
        self.source
    }

    pub fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::EOS)
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current].0
    }

    pub fn peek_position(&self) -> [usize; 2] {
        self.tokens[self.current].1
    }

    pub fn peek_previous(&self) -> &Token {
        if self.current > 0 {
            &self.tokens[self.current - 1].0
        } else {
            &Token::EOS
        }
    }

    pub fn peek_next(&self, offset: usize) -> &Token {
        let index = self.current + offset;
        if index < self.tokens.len() {
            &self.tokens[index].0
        } else {
            &Token::EOS
        }
    }

    pub fn advance(&mut self) -> bool {
        if self.current < self.tokens.len() {
            self.current += 1;
            true
        } else {
            false
        }
    }

    pub fn check(&self, token_type: &Token) -> bool {
        &self.tokens[self.current].0 == token_type
    }

    pub fn consume(&mut self, token_type: &Token) -> bool {
        if self.current < self.tokens.len() && &self.tokens[self.current].0 == token_type {
            self.current += 1;
            true
        } else {
            false
        }
    }

    pub fn previous_line_terminator(&self) -> bool {
        if self.current > 0 && self.current < self.tokens.len() {
            let prev_line = self.tokens[self.current - 1].1[0];
            let curr_line = self.tokens[self.current].1[0];
            prev_line < curr_line
        } else {
            false
        }
    }
    
    pub fn save_position(&self) -> usize {
        self.current
    }
    
    pub fn restore_position(&mut self, position: usize) {
        self.current = position;
    }
}
