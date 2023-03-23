#![allow(dead_code)]
#![allow(unused_variables)]
#![feature(box_patterns)]
#![feature(map_try_insert)]

mod ast_eval;
mod ast_ir;
mod context;
mod error;
mod lexer;
mod parser;

pub use ast_eval::ASTNodeEval;
pub use ast_ir::ASTNodeGen;
pub use context::Context;
pub use error::{Pl0Error, Result};
pub use lexer::{KeywordToken, Lexer, OperatorToken, Token};
pub use parser::{
    ASTNode, BlockASTNode, CondASTNode, ExprASTNode, FactorASTNode, Parser, StmtASTNode,
    TermASTNode,
};
