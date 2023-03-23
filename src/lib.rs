#![allow(dead_code)]
#![allow(unused_variables)]
#![feature(box_patterns)]
#![feature(map_try_insert)]

mod error;
mod interpreter;
mod lexer;
mod parser;

pub use error::{Pl0Error, Result};
pub use lexer::{KeywordToken, Lexer, OperatorToken, Token};
pub use parser::{
    ASTNode, BlockASTNode, CondASTNode, ExprASTNode, FactorASTNode, Parser, StmtASTNode,
    TermASTNode,
};
