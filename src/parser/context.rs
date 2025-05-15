use std::collections::HashSet;
use crate::lexer::LexicalContext;

pub struct ParserContext {
    // TODO to lexical context
    pub in_strict_mode: bool,
    pub labels: HashSet<Box<str>>,
    pub context_stack: Vec<LexicalContext>,
}

impl ParserContext {
    pub fn new() -> Self {
        Self {
            in_strict_mode: false,
            labels: HashSet::new(),
            context_stack: vec![LexicalContext::Default],
        }
    }

    pub fn push_context(&mut self, context: LexicalContext) {
        self.context_stack.push(context);
    }

    pub fn pop_context(&mut self) {
        if self.context_stack.len() > 1 {
            self.context_stack.pop();
        }
    }

    fn current_context(&self) -> &LexicalContext {
        self.context_stack.last().unwrap_or(&LexicalContext::Default)
    }

    pub fn is_in_loop_body(&self) -> bool {
        matches!(self.current_context(), LexicalContext::LoopBody)
    }
    
    pub fn is_in_switch(&self) -> bool {
        matches!(self.current_context(), LexicalContext::SwitchBody)
    }
    
    pub fn is_in_function(&self) -> bool {
        self.context_stack.iter().any(|ctx| matches!(ctx, LexicalContext::FunctionBody { .. }))
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
