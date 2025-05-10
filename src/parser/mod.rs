//! JavaScript parser module
//! 
//! This module provides a composable, extensible parser for JavaScript code.

mod asi;
mod error;
mod stream;
mod context;
mod parser;
mod combinator;

// Public exports
pub use self::parser::Parser;
pub use self::combinator::ParserCombinator;
pub use self::error::{ParserError, ParseResult};