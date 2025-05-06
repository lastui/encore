use std::fmt;
use crate::lexer::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexicalContext {
    Default,
    PropertyKey,
    MemberAccess,
    ImportExport,
    ObjectPattern,
    ParameterName { strict_mode: bool },
    FunctionBody { allow_yield: bool, allow_await: bool },
    LoopParameters,
    LoopBody,
    SwitchBody,
}

impl fmt::Display for LexicalContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default => write!(f, "global"),
            Self::PropertyKey => write!(f, "property key"),
            Self::MemberAccess => write!(f, "member access"),
            Self::ImportExport => write!(f, "import export"),
            Self::ObjectPattern => write!(f, "object pattern"),
            Self::ParameterName { strict_mode: false } => write!(f, "param name"),
            Self::ParameterName { strict_mode: true } => write!(f, "strict param name"),
            Self::FunctionBody { allow_yield: false, allow_await: false } => write!(f, "function body"),
            Self::FunctionBody { allow_yield: true, allow_await: false } => write!(f, "generator function body"),
            Self::FunctionBody { allow_yield: false, allow_await: true } => write!(f, "async function body"),
            Self::FunctionBody { allow_yield: true, allow_await: true } => write!(f, "async generator function body"),
            Self::LoopParameters => write!(f, "loop init"),
            Self::LoopBody => write!(f, "loop body"),
            Self::SwitchBody => write!(f, "switch body"),
        }
    }
}

impl LexicalContext {

    // Fast check if this context allows any keywords as identifiers
    pub fn has_keywords_as_identifiers(&self) -> bool {
        match self {
            LexicalContext::Default => false,
            _ => true,
        }
    }

    pub fn allows_token_as_identifier(&self, token: &Token) -> bool {
        match self {
            // In property contexts, all keywords can be identifiers except a few special ones
            Self::MemberAccess => {

                //let result = matches!(keyword, "default");

                println!("Checking in MemberAccess with {:#?}", token);

                if token == &Token::Default {
                    true
                } else if token == &Token::From {
                    true
                } else if token == &Token::For {
                    true
                } else if token == &Token::Get {
                    true
                } else {
                    false
                }

                //result
                //false
            },
            Self::PropertyKey => {
                //println!("Currently in PropertyKey with {:#?}", keyword);
                false
            },

            // In import/export contexts, specific keywords are allowed as identifiers
            Self::ImportExport => {
                //println!("Currently in ImportExport with {:#?}", keyword);
                false
            },
            
            // In object patterns, allow destructuring with keywords except special ones
            Self::ObjectPattern => {
                //println!("Currently in ObjectPattern with {:#?}", keyword);
                false
            },
            
            // In parameter names, most keywords can be identifiers in non-strict mode
            Self::ParameterName { strict_mode } => {
                //println!("Currently in ParameterName strict={:#?} with {:#?}", strict_mode, keyword);
                if *strict_mode {
                    false
                } else {
                    false
                }
            },
            Self::LoopParameters => {
                //println!("Currently in LoopParameters with {:#?}", token);
                if token == &Token::Set {
                    true
                } else {
                    false
                }
            },
            // In function bodies, yield and await have special handling
            Self::FunctionBody { allow_yield, allow_await } => {
                //println!("Currently in FunctionBody with {:#?}", keyword);

                if (*allow_yield && token == &Token::Yield) || (*allow_await && token == &Token::Await) {
                    false
                } else if token == &Token::As {
                    true
                } else {
                    // Default to not allowing keywords as identifiers in function bodies
                    false
                }
            },
            
            // In loop bodies, break and continue are special
            Self::LoopBody => {
                //println!("Currently in LoopBody with {:#?}", keyword);
                false
            },
            
            // In switch bodies, case and default are special
            Self::SwitchBody => {
                //println!("Currently in SwitchBody with {:#?}", keyword);
                false
            },
            
            // In default context, keywords are not identifiers
            Self::Default => {
                //println!("Currently in Default with {:#?}", keyword);
                false
            },
        }
    }
}
