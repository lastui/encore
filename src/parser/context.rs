use std::collections::HashSet;
use crate::lexer::LexicalContext;

/// Maintains parser state and context information
pub struct ParserContext {
    pub in_strict_mode: bool,
    pub labels: HashSet<Box<str>>,
    pub context_stack: Vec<LexicalContext>,
    pub comments: Vec<crate::ast::Comment>,
}

impl ParserContext {
    pub fn new() -> Self {
        Self {
            in_strict_mode: false,
            labels: HashSet::new(),
            context_stack: vec![LexicalContext::Default],
            comments: Vec::new(),
        }
    }

    pub fn current_context(&self) -> &LexicalContext {
        self.context_stack.last().unwrap_or(&LexicalContext::Default)
    }

    pub fn push_context(&mut self, context: LexicalContext) {
        self.context_stack.push(context);
    }

    pub fn pop_context(&mut self) {
        if self.context_stack.len() > 1 {
            self.context_stack.pop();
        }
    }

    pub fn has_context<F>(&self, predicate: F) -> bool 
    where 
        F: Fn(&LexicalContext) -> bool 
    {
        self.context_stack.iter().any(predicate)
    }

    pub fn is_in_loop_body(&self) -> bool {
        self.has_context(|ctx| matches!(ctx, LexicalContext::LoopBody))
    }
    
    pub fn is_in_switch(&self) -> bool {
        self.has_context(|ctx| matches!(ctx, LexicalContext::SwitchBody))
    }
    
    pub fn is_in_function(&self) -> bool {
        self.has_context(|ctx| matches!(ctx, LexicalContext::FunctionBody { .. }))
    }

    pub fn allows_yield(&self) -> bool {
        matches!(self.current_context(), LexicalContext::FunctionBody { allow_yield: true, .. })
    }

    pub fn allows_await(&self) -> bool {
        matches!(self.current_context(), LexicalContext::FunctionBody { allow_await: true, .. })
    }
    
    pub fn get_context_stack_info(&self) -> Vec<String> {
        let depth = 10;
        let stack_len = self.context_stack.len();
        let start_idx = if stack_len > depth { stack_len - depth } else { 0 };

        self.context_stack[start_idx..]
            .iter()
            .rev()
            .map(|ctx| format!("{}", ctx))
            .collect()
    }
}
