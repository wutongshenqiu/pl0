use crate::{Pl0Error, Result};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorToken {
    // Single
    Equal,   // =
    Hashtag, // #
    Plus,    // +
    Minus,   // -
    Mul,     // *
    Div,     // /
    Comma,   // ,
    Semi,    // ;
    Dot,     // .
    LPar,    // (
    RPar,    // )
    Less,    // <
    Greater, // >
    Input,   // ?
    Output,  // !

    // double
    LEqual,     // <=
    GEqual,     // >=
    Assignment, // :=
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum KeywordToken {
    Const,
    Var,
    Procedure,
    Call,
    Begin,
    End,
    If,
    Then,
    While,
    Do,
    Odd,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(i64),
    Operator(OperatorToken),
    Ident(String),
    Keyword(KeywordToken),
    Eof,
}

impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            // operator
            "=" => Token::Operator(OperatorToken::Equal),
            "#" => Token::Operator(OperatorToken::Hashtag),
            "+" => Token::Operator(OperatorToken::Plus),
            "-" => Token::Operator(OperatorToken::Minus),
            "*" => Token::Operator(OperatorToken::Mul),
            "/" => Token::Operator(OperatorToken::Div),
            "," => Token::Operator(OperatorToken::Comma),
            ";" => Token::Operator(OperatorToken::Semi),
            "." => Token::Operator(OperatorToken::Dot),
            "(" => Token::Operator(OperatorToken::LPar),
            ")" => Token::Operator(OperatorToken::RPar),
            ">" => Token::Operator(OperatorToken::Greater),
            "<" => Token::Operator(OperatorToken::Less),
            "?" => Token::Operator(OperatorToken::Input),
            "!" => Token::Operator(OperatorToken::Output),
            "<=" => Token::Operator(OperatorToken::LEqual),
            ">=" => Token::Operator(OperatorToken::GEqual),
            ":=" => Token::Operator(OperatorToken::Assignment),

            // keyword
            "const" => Token::Keyword(KeywordToken::Const),
            "var" => Token::Keyword(KeywordToken::Var),
            "procedure" => Token::Keyword(KeywordToken::Procedure),
            "call" => Token::Keyword(KeywordToken::Call),
            "begin" => Token::Keyword(KeywordToken::Begin),
            "end" => Token::Keyword(KeywordToken::End),
            "if" => Token::Keyword(KeywordToken::If),
            "then" => Token::Keyword(KeywordToken::Then),
            "while" => Token::Keyword(KeywordToken::While),
            "do" => Token::Keyword(KeywordToken::Do),
            "odd" => Token::Keyword(KeywordToken::Odd),

            // ident
            x => Token::Ident(x.into()),
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl From<i64> for Token {
    fn from(value: i64) -> Self {
        Token::Number(value)
    }
}

fn is_ident_start_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start_char(c) || c.is_ascii_digit()
}

fn is_op_start(c: char) -> bool {
    c == '='
        || c == '#'
        || c == '+'
        || c == '-'
        || c == '*'
        || c == '/'
        || c == ','
        || c == ';'
        || c == '.'
        || c == '('
        || c == ')'
        || c == ':'
        || c == '>'
        || c == '<'
        || c == '?'
        || c == '!'
}

pub struct Lexer {
    src: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        Self {
            src: src.chars().collect(),
            pos: 0,
        }
    }

    pub fn get_next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        if self.is_end() {
            Ok(Token::Eof)
        } else {
            match self.ch()? {
                x if x.is_ascii_digit() => self.number(),
                x if is_ident_start_char(x) => self.ident(),
                x if is_op_start(x) => self.op(),
                x => Err(Pl0Error::UnexpectedChar {
                    ch: x,
                    pos: self.pos,
                }),
            }
        }
    }

    pub fn peek_next_token(&mut self) -> Result<Token> {
        let old_pos = self.pos;
        let token = self.get_next_token()?;
        self.pos = old_pos;

        Ok(token)
    }
}

impl Lexer {
    fn is_end(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn skip_whitespace(&mut self) {
        while !self.is_end() && self.src[self.pos].is_whitespace() {
            self.pos += 1;
        }
    }

    fn ch(&self) -> Result<char> {
        if self.is_end() {
            Err(Pl0Error::EndOfSource)
        } else {
            Ok(self.src[self.pos])
        }
    }

    fn number(&mut self) -> Result<Token> {
        let mut s = Vec::new();
        while !self.is_end() && self.ch()?.is_ascii_digit() {
            s.push(self.ch()?);
            self.pos += 1;
        }

        let value: i64 = s.into_iter().collect::<String>().parse()?;

        Ok(value.into())
    }

    fn ident(&mut self) -> Result<Token> {
        let mut s = Vec::new();
        while !self.is_end() && is_ident_char(self.ch()?) {
            s.push(self.ch()?);
            self.pos += 1;
        }

        let s: String = s.into_iter().collect();
        Ok(s.into())
    }

    fn op(&mut self) -> Result<Token> {
        let s = self.ch()?;
        self.pos += 1;

        if s == ':' {
            if self.is_end() {
                Err(Pl0Error::UnexpectedEof)
            } else if self.ch()? != '=' {
                Err(Pl0Error::UnexpectedChar {
                    ch: self.ch()?,
                    pos: self.pos,
                })
            } else {
                self.pos += 1;
                Ok(":=".into())
            }
        } else if (s == '>' || s == '<') && (!self.is_end() && self.ch()? == '=') {
            self.pos += 1;
            Ok(format!("{}=", s).into())
        } else {
            Ok(format!("{}", s).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ch() {
        let src = "1";
        let lexer = Lexer::new(src);

        assert!(matches!(lexer.ch(), Ok(t) if t == '1'));
    }

    #[test]
    fn test_ch_err() {
        let src = "";
        let lexer = Lexer::new(src);

        assert!(matches!(lexer.ch(), Err(Pl0Error::EndOfSource)));
    }

    #[test]
    fn test_number() {
        let src = "1234";
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.number(), Ok(Token::Number(t)) if t == 1234));
    }

    #[test]
    fn test_number_err() {
        let src = "abcd";
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.number(), Err(Pl0Error::ParseInt(_))))
    }

    #[test]
    fn test_ident() {
        let src = "abcd";
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.ident(), Ok(Token::Ident(t)) if t == "abcd"))
    }

    #[test]
    fn test_op() {
        for src in vec![
            "=", "#", "+", "-", "*", "/", ",", ";", ".", "(", ")", ">", "<", "?", "!", ">=", "<=",
            ":=",
        ] {
            let mut lexer = Lexer::new(src);
            assert!(matches!(lexer.op(), Ok(Token::Operator(_))))
        }
    }

    #[test]
    fn test_op_err() {
        let src = ":";
        let mut lexer = Lexer::new(src);
        assert!(matches!(lexer.op(), Err(Pl0Error::UnexpectedEof)));

        let src = ":*";
        let mut lexer = Lexer::new(src);
        assert!(matches!(
            lexer.op(),
            Err(Pl0Error::UnexpectedChar { ch: '*', pos: 1 })
        ));
    }

    #[test]
    fn test_procedure() {
        let src = "
        procedure P;
        a := 1;
        b := 1
        .
        ";
        let mut lexer = Lexer::new(src);

        // Procedure P;
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // a := 1;
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // b := 1.
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        assert!(matches!(lexer.get_next_token(), Ok(Token::Eof)));
    }

    #[test]
    fn test_invalid_token() {
        let src = "不合法的token";
        let mut lexer = Lexer::new(src);
        assert!(matches!(
            lexer.get_next_token(),
            Err(Pl0Error::UnexpectedChar { ch: '不', pos: 0 })
        ));
    }

    #[test]
    fn test_peek_next_token() {
        let src = "var i";
        let mut lexer = Lexer::new(src);
        assert!(matches!(lexer.peek_next_token(), Ok(Token::Keyword(_))));
        assert!(matches!(lexer.peek_next_token(), Ok(Token::Keyword(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        assert!(matches!(lexer.peek_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.peek_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));

        assert!(matches!(lexer.peek_next_token(), Ok(Token::Eof)));
        assert!(matches!(lexer.peek_next_token(), Ok(Token::Eof)));
    }

    #[test]
    fn test_basic() {
        let src = "var i, s;
        begin
            i := 0; s := 0;
            while i < 5 do
            begin
                i := i + 1;
                s := s + i * i
            end
        end.";
        let mut lexer = Lexer::new(src);

        // var i, s;
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // begin
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        // i := 0, s := 0;
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // while i < 5 do
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        // begin
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        // i := i + 1
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Number(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // s := s + i * i
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Ident(_))));

        // end
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        // end
        assert!(matches!(lexer.get_next_token(), Ok(Token::Keyword(_))));

        // .
        assert!(matches!(lexer.get_next_token(), Ok(Token::Operator(_))));

        // Eof
        assert!(matches!(lexer.get_next_token(), Ok(Token::Eof)));
        assert!(matches!(lexer.get_next_token(), Ok(Token::Eof)));
    }
}
