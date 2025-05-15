use crate::ast::*;
use crate::lexer::{Token, LexicalContext};
use super::error::{ParserError, ParseResult};
use super::stream::TokenStream;
use super::combinator::ParserCombinator;
use super::context::ParserContext;
use crate::grammar::*;

/*
use std::borrow::Cow;

pub struct TokenAccess<'a> {
    token: Cow<'a, Token>,
}

impl<'a> std::ops::Deref for TokenAccess<'a> {
    type Target = Token;
    
    fn deref(&self) -> &Self::Target {
        self.token.as_ref()
    }
}*/

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    context: ParserContext,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [(Token, [usize; 2])]) -> Self {
        Self {
            stream: TokenStream::new(tokens),
            context: ParserContext::new(),
        }
    }
    
    // Main parsing methods
    pub fn parse_module(&mut self) -> ParseResult<Program> {
        ModuleNode::new().parse(self)
    }
    
    pub fn parse_script(&mut self) -> ParseResult<Program> {
        ScriptNode::new().parse(self)
    }
    
    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        ExpressionNode::new().parse(self)
    }
    
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        StatementNode::new().parse(self)
    }
    
    // Source handling
    pub fn attach_source(&mut self, source: &'a str) {
        self.stream.attach_source(source);
    }
    
    pub fn get_source_text(&self) -> &str {
        self.stream.get_source_text()
    }
    
    // Error handling
    pub fn error_at_current(&self, message: &str) -> ParserError {
        ParserError::at_current(self, message)
    }
    
    // TokenStream delegations
    pub fn is_at_end(&self) -> bool {
        self.stream.is_at_end()
    }
    
    /*
    #[inline(always)]
    fn peek_internal(&self) -> Option<TokenAccess<'_>> {
        self.stream.peek().map(|token| {
            let cow = if !matches!(token, Token::Identifier(_)) && self.current_context().allows_token_as_identifier(token) {
                if let Some(text) = token.keyword_text() {
                    Cow::Owned(Token::Identifier(text.to_string()))
                } else {
                    Cow::Borrowed(token)
                }
            } else {
                Cow::Borrowed(token)
            };
            
            TokenAccess { token: cow }
        })

    }
    */

    pub fn peek(&self) -> &Token {
        self.stream.peek()
    }
    
    pub fn peek_previous(&self) -> &Token {
        self.stream.peek_previous()
    }
    
    pub fn peek_next(&self, offset: usize) -> &Token {
        self.stream.peek_next(offset)
    }
    
    pub fn advance(&mut self) -> bool {
        self.stream.advance()
    }
    
    pub fn check(&self, token_type: &Token) -> bool {
        self.stream.check(token_type)
    }
    
    pub fn consume(&mut self, token_type: &Token) -> bool {
        self.stream.consume(token_type)
    }
    
    pub fn previous_line_terminator(&self) -> bool {
        self.stream.previous_line_terminator()
    }
    
    pub fn save_position(&self) -> usize {
        self.stream.save_position()
    }
    
    pub fn restore_position(&mut self, position: usize) {
        self.stream.restore_position(position)
    }
    
    pub fn get_current_position(&self) -> [usize; 2] {
        self.stream.peek_position()
    }
    
    pub fn assert_consume(&mut self, token_type: &Token, message: &str) -> ParseResult<&Token> {
        if self.consume(token_type) {
            Ok(self.peek_previous())
        } else {
            Err(self.error_at_current(message))
        }
    }
        
    pub fn get_token_stack_info(&self) -> Vec<String> {
        self.stream.get_token_stack_info()
    }

    // ParserContext delegations
    pub fn get_context_stack_info(&self) -> Vec<String> {
        self.context.get_context_stack_info()
    }
    
    pub fn is_in_function(&self) -> bool {
        self.context.is_in_function()
    }
    
    pub fn is_in_loop_body(&self) -> bool {
        self.context.is_in_loop_body()
    }
    
    pub fn is_in_switch(&self) -> bool {
        self.context.is_in_switch()
    }
    
    pub fn allows_yield(&self) -> bool {
        self.context.allows_yield()
    }
    
    pub fn allows_await(&self) -> bool {
        self.context.allows_await()
    }
    
    // TODO maybe
    /*

    pub struct ContextGuard<'a, 'b> {
        parser: &'a mut Parser<'b>,
    }

    impl<'a, 'b> Drop for ContextGuard<'a, 'b> {
        fn drop(&mut self) {
            self.parser.pop_context();
        }
    }

    impl<'a> Parser<'a> {
        pub fn with_context_guard(&mut self, context: LexicalContext) -> ContextGuard<'_, 'a> {
            self.push_context(context);
            ContextGuard { parser: self }
        }
    }
    */

    pub fn with_context<F, R>(&mut self, context: LexicalContext, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        self.context.push_context(context);
        let result = f(self);
        self.context.pop_context();
        result
    }
    
    // Label management
    pub fn add_label(&mut self, label: Box<str>) {
        self.context.labels.insert(label);
    }
    
    pub fn remove_label(&mut self, label: &str) {
        self.context.labels.remove(label);
    }
    
    pub fn has_label(&self, label: &str) -> bool {
        self.context.labels.contains(label)
    }
    
    // Strict mode handling
    pub fn set_strict_mode(&mut self, strict: bool) {
        self.context.in_strict_mode = strict;
    }
    
    pub fn is_strict_mode(&self) -> bool {
        self.context.in_strict_mode
    }

}


/*
     #[inline(always)]
    fn coalesce_identifier<'t>(&self, token: &'t mut Token) {
        // Only transform if not already an identifier and context allows it
        if !matches!(token, Token::Identifier(_)) && self.current_context().allows_token_as_identifier(token) {
            if let Some(text) = token.keyword_text() {
                // Transform the token in place to an identifier
                *token = Token::Identifier(text.to_string());
            }
        }
    }
    */