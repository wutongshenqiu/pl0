use thiserror::Error;

use crate::Token;

// TODO: 把 lexer 和 parser 的错误错分开或许更清晰, 但没找到 thiserror 中相关的用法?
#[derive(Error, Debug)]
// https://github.com/dtolnay/thiserror/issues/35
#[error("{:#?}", self)]
pub enum Pl0Error {
    // Lexer
    ParseInt(#[from] std::num::ParseIntError),
    EndOfSource,
    #[error("unexpected char `{}` at pos {}", ch, pos)]
    UnexpectedChar {
        ch: char,
        pos: usize,
    },
    UnexpectedEof,

    // Parser
    #[error("invalid token {:#?}", .0)]
    InvalidToken(Token),
    #[error("expect token to be {:#?}, but got {:#?}", token, expected_token)]
    UnexpectedToken {
        token: Token,
        expected_token: Token,
    },

    // interpreter
    #[error("variable `{0}` used before initialize")]
    VarUsedBeforeInitialize(String),
    #[error("symbol `{0}` is undefined")]
    UndefinedSymbol(String),
    #[error("variable `{0}` redefined")]
    RedefinedVar(String),
    #[error("const `{0}` redefined")]
    RedefinedConst(String),
    #[error("invalid term")]
    InvalidTerm,
    #[error("invalid term operator")]
    InvalidTermOp,
    #[error("division by zero")]
    DivisionByZero,
    #[error("invalid expr")]
    InvalidExpr,
    #[error("invalid expr operator")]
    InvalidExprOp,
    #[error("invalid odd condition")]
    InvalidOddCond,
    #[error("invalid standard condition")]
    InvalidStdCond,
    #[error("invalid standard condition operator")]
    InvalidStdCondOp,
    #[error("invalid assignment")]
    InvalidAssign,
    #[error("invalid if")]
    InvalidIf,
    #[error("invalid while")]
    InvalidWhile,
    #[error("empty ast node")]
    EmptyASTNode,
}

pub type Result<T> = std::result::Result<T, Pl0Error>;
