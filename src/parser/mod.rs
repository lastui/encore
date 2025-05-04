//! JavaScript parser module
//! 
//! This module contains the parser for JavaScript code.
//! It has been split into multiple files for better maintainability.

mod error;
mod state;
mod core;
mod expressions;
mod statements;
mod declarations;
mod patterns;
mod functions;
mod classes;
mod modules;


pub use self::core::Parser;
