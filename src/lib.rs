#![allow(dead_code)]
#![allow(unused_variables)]
#![feature(box_patterns)]

mod error;
mod lexer;
mod parser;

pub use error::{Pl0Error, Result};
pub use lexer::{KeywordToken, Lexer, OperatorToken, Token};
