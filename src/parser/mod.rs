//! JavaScript parser module
//! 
//! This module contains the parser for JavaScript code.
//! It has been split into multiple files for better maintainability.

mod error;
mod state;
mod core;
mod expressions;
mod statements;
mod patterns;
mod functions;
mod classes;
mod modules;
mod asi;

pub use self::core::Parser;

mod prelude;

pub use prelude::*;