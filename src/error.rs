use thiserror::Error;

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
}

pub type Result<T> = std::result::Result<T, Pl0Error>;
