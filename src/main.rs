#![allow(dead_code)]
#![allow(unused_variables)]

#[derive(Debug, PartialEq)]
enum Token {
    Number { value: i32 },
    Operator { op: String },
    Ident { name: String },
    Keyword { name: String },
    Eof,
}

impl Token {
    pub fn number(value: i32) -> Self {
        Self::Number { value }
    }

    pub fn operator(op: String) -> Self {
        Self::Operator { op }
    }

    pub fn ident(name: String) -> Self {
        Self::Ident { name }
    }

    pub fn keyword(name: String) -> Self {
        Self::Keyword { name }
    }
}

fn is_ident_start_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start_char(c) || c.is_ascii_digit()
}

fn is_single_op(c: char) -> bool {
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
}

fn is_double_op_start_char(c: char) -> bool {
    c == ':' || c == '>' || c == '<'
}

fn is_keyword(name: &str) -> bool {
    name == "const"
        || name == "var"
        || name == "procedure"
        || name == "call"
        || name == "begin"
        || name == "end"
        || name == "if"
        || name == "then"
        || name == "while"
        || name == "do"
        || name == "odd"
}

struct Lexer {
    src: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        Self {
            src: src.chars().collect(),
            pos: 0,
        }
    }

    pub fn get_next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_end() {
            return Token::Eof;
        }

        // Number
        match self.ch() {
            x if x.is_ascii_digit() => self.number(),
            x if is_ident_start_char(x) => self.ident(),
            x if is_single_op(x) => self.single_op(),
            x if is_double_op_start_char(x) => self.double_op(),
            _ => panic!("Unexpected char {}", self.ch()),
        }
    }

    pub fn peek_next_token(&mut self) -> Token {
        let old_pos = self.pos;
        let token = self.get_next_token();
        self.pos = old_pos;

        token
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

    fn ch(&self) -> char {
        self.src[self.pos]
    }

    fn number(&mut self) -> Token {
        let mut s = Vec::new();
        while !self.is_end() && self.ch().is_ascii_digit() {
            s.push(self.ch());
            self.pos += 1;
        }

        let s: String = s.into_iter().collect();
        let value: i32 = s.parse().unwrap();

        Token::number(value)
    }

    fn ident(&mut self) -> Token {
        let mut s = Vec::new();
        while !self.is_end() && is_ident_char(self.ch()) {
            s.push(self.ch());
            self.pos += 1;
        }

        let s: String = s.into_iter().collect();
        if is_keyword(&s) {
            Token::keyword(s)
        } else {
            Token::ident(s)
        }
    }

    fn single_op(&mut self) -> Token {
        let c = self.ch();
        self.pos += 1;

        Token::operator(c.into())
    }

    fn double_op(&mut self) -> Token {
        let s = self.ch();
        self.pos += 1;

        if s == ':' {
            if self.is_end() || self.ch() != '=' {
                panic!("Expect =");
            }
            self.pos += 1;
            Token::operator(":=".into())
        } else if s == '>' || s == '<' {
            if !self.is_end() && self.ch() == '=' {
                self.pos += 1;
                Token::operator(format!("{}=", s))
            } else {
                Token::operator(s.into())
            }
        } else {
            panic!("Invalid character {}", self.ch())
        }
    }
}

#[derive(Debug)]
enum FactorASTNode {
    Ident(String),
    Number(i32),
    Expr(Box<ASTNode>),
}

#[derive(Debug)]
enum CondASTNode {
    OddCond(Box<ASTNode>),
    StdCond {
        op: String,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
}

#[derive(Debug)]
enum StmtASTNode {
    Assign {
        lhs: String,
        rhs: Box<ASTNode>,
    },
    Call(String),
    #[allow(clippy::vec_box)]
    Begin(Vec<Box<ASTNode>>),
    If {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    While {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
}

#[derive(Debug)]
enum ASTNode {
    Program(Box<ASTNode>),
    Block {
        consts: Vec<(String, i32)>,
        vars: Vec<String>,
        procedures: Vec<(String, Box<ASTNode>)>,
        stmt: Box<ASTNode>,
    },
    Stmt(StmtASTNode),
    Cond(CondASTNode),
    Expr {
        op: String,
        lhs: Box<ASTNode>,
        rhs: Vec<(String, Box<ASTNode>)>,
    },
    Term {
        lhs: Box<ASTNode>,
        rhs: Vec<(String, Box<ASTNode>)>,
    },
    Factor(FactorASTNode),
}

struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Box<ASTNode> {
        self.program()
    }
}

impl Parser {
    fn expect(&mut self, token: Token) {
        let tk = self.lexer.get_next_token();

        if tk != token {
            panic!("expect token {:#?}, but got {:#?}", tk, token)
        }
    }

    fn next_ident(&mut self) -> String {
        let tk = self.lexer.get_next_token();

        match tk {
            Token::Ident { name } => name,
            x => panic!("expect ident token, but got {:#?}", x),
        }
    }

    fn next_number(&mut self) -> i32 {
        let tk = self.lexer.get_next_token();

        match tk {
            Token::Number { value } => value,
            x => panic!("expect number token, but got {:#?}", x),
        }
    }

    fn program(&mut self) -> Box<ASTNode> {
        let res = Box::new(ASTNode::Program(self.block()));
        self.expect(Token::operator(".".into()));

        res
    }

    fn block(&mut self) -> Box<ASTNode> {
        let mut consts = Vec::new();
        let mut vars = Vec::new();
        let mut procedures = Vec::new();

        if let Token::Keyword { name } = self.lexer.peek_next_token() {
            if name == "const" {
                self.expect(Token::keyword("const".into()));
                let ident = self.next_ident();
                self.expect(Token::operator("=".into()));
                let number = self.next_number();
                consts.push((ident, number));

                loop {
                    match self.lexer.get_next_token() {
                        Token::Operator { op } if op == ";" => break,
                        Token::Operator { op } if op == "," => {
                            let ident = self.next_ident();
                            self.expect(Token::operator("=".into()));
                            let number = self.next_number();
                            consts.push((ident, number));
                        }
                        x => panic!("unexpected token {:#?}", x),
                    }
                }
            }
        }

        if let Token::Keyword { name } = self.lexer.peek_next_token() {
            if name == "var" {
                self.expect(Token::keyword("var".into()));
                let ident = self.next_ident();
                vars.push(ident);
                loop {
                    match self.lexer.get_next_token() {
                        Token::Operator { op } if op == ";" => break,
                        Token::Operator { op } if op == "," => {
                            let ident = self.next_ident();
                            vars.push(ident);
                        }
                        x => panic!("unexpected token {:#?}", x),
                    }
                }
            }
        }

        loop {
            match self.lexer.peek_next_token() {
                Token::Keyword { name } if name == "procedure" => {
                    self.expect(Token::keyword("procedure".into()));
                    let ident = self.next_ident();
                    self.expect(Token::operator(";".into()));
                    let block = self.block();
                    procedures.push((ident, block));
                }
                _ => break,
            }
        }

        let stmt = self.stmt();

        Box::new(ASTNode::Block {
            consts,
            vars,
            procedures,
            stmt,
        })
    }

    fn stmt(&mut self) -> Box<ASTNode> {
        match self.lexer.get_next_token() {
            Token::Ident { name } => {
                self.expect(Token::operator(":=".into()));
                let rhs = self.expr();

                Box::new(ASTNode::Stmt(StmtASTNode::Assign { lhs: name, rhs }))
            }
            Token::Keyword { name } => match name.as_str() {
                "call" => {
                    let ident = self.next_ident();
                    Box::new(ASTNode::Stmt(StmtASTNode::Call(ident)))
                }
                "begin" => {
                    let mut stmts = Vec::new();
                    stmts.push(self.stmt());

                    loop {
                        match self.lexer.peek_next_token() {
                            Token::Operator { op } if op == ";" => {
                                self.expect(Token::operator(";".into()));
                                stmts.push(self.stmt());
                            }
                            _ => break,
                        }
                    }

                    self.expect(Token::Keyword { name: "end".into() });

                    Box::new(ASTNode::Stmt(StmtASTNode::Begin(stmts)))
                }
                "if" => {
                    let lhs = self.cond();
                    self.expect(Token::keyword("then".into()));
                    let rhs = self.stmt();

                    Box::new(ASTNode::Stmt(StmtASTNode::If { lhs, rhs }))
                }
                "while" => {
                    let lhs = self.cond();
                    self.expect(Token::keyword("do".into()));
                    let rhs = self.stmt();

                    Box::new(ASTNode::Stmt(StmtASTNode::While { lhs, rhs }))
                }
                x => panic!("unexpect token {:#?}", x),
            },
            x => panic!("unexpected token {:#?}", x),
        }
    }

    fn cond(&mut self) -> Box<ASTNode> {
        match self.lexer.peek_next_token() {
            Token::Keyword { .. } => {
                self.expect(Token::keyword("odd".into()));
                let expr = self.expr();

                Box::new(ASTNode::Cond(CondASTNode::OddCond(expr)))
            }
            _ => {
                let lhs = self.expr();
                let op = match self.lexer.get_next_token() {
                    Token::Operator { op }
                        if op == "="
                            || op == "#"
                            || op == "<"
                            || op == "<="
                            || op == ">"
                            || op == ">=" =>
                    {
                        op
                    }
                    x => panic!("unexpected token {:#?}", x),
                };
                let rhs = self.expr();

                Box::new(ASTNode::Cond(CondASTNode::StdCond { op, lhs, rhs }))
            }
        }
    }

    fn expr(&mut self) -> Box<ASTNode> {
        let op = match self.lexer.peek_next_token() {
            Token::Operator { op } if op == "-" => "-".into(),
            _ => "+".into(),
        };

        let lhs = self.term();
        let mut rhs = Vec::new();

        loop {
            match self.lexer.peek_next_token() {
                Token::Operator { op } if op == "+" || op == "-" => {
                    self.expect(Token::operator(op.clone()));
                    rhs.push((op, self.term()));
                }
                _ => break,
            }
        }

        Box::new(ASTNode::Expr { op, lhs, rhs })
    }

    fn term(&mut self) -> Box<ASTNode> {
        let lhs = self.factor();
        let mut rhs = Vec::new();

        loop {
            match self.lexer.peek_next_token() {
                Token::Operator { op } if op == "*" || op == "/" => {
                    self.expect(Token::operator(op.clone()));
                    rhs.push((op, self.factor()));
                }
                _ => break,
            }
        }

        Box::new(ASTNode::Term { lhs, rhs })
    }

    fn factor(&mut self) -> Box<ASTNode> {
        match self.lexer.get_next_token() {
            Token::Ident { name } => Box::new(ASTNode::Factor(FactorASTNode::Ident(name))),
            Token::Number { value } => Box::new(ASTNode::Factor(FactorASTNode::Number(value))),
            Token::Operator { op } => {
                if op != "(" {
                    panic!("expect op to be `(`, but got `{}`", op);
                }

                let expr = self.expr();
                self.expect(Token::operator(")".into()));

                Box::new(ASTNode::Factor(FactorASTNode::Expr(expr)))
            }
            x => panic!("unexpected token {:#?}", x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_eof() {
        let src = "".into();
        let mut lexer = Lexer::new(src);

        for _ in 0..10 {
            assert!(matches!(lexer.get_next_token(), Token::Eof))
        }
    }

    #[test]
    fn token_number() {
        let src = "12345".into();
        let mut lexer = Lexer::new(src);

        assert!(matches!(
            lexer.get_next_token(),
            Token::Number { value: 12345 }
        ));
        assert!(matches!(lexer.get_next_token(), Token::Eof));
    }

    #[test]
    fn token_keyword() {
        let src = "const".into();
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.get_next_token(), Token::Keyword { .. }));
        assert!(matches!(lexer.get_next_token(), Token::Eof));
    }

    #[test]
    fn token_ident() {
        let src = "ab_asd31".into();
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.get_next_token(), Token::Ident { .. }));
        assert!(matches!(lexer.get_next_token(), Token::Eof));
    }

    #[test]
    fn token_single_op() {
        let src = ",".into();
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.get_next_token(), Token::Operator { .. }));
        assert!(matches!(lexer.get_next_token(), Token::Eof));
    }

    #[test]
    fn token_double_op() {
        let src = ":=".into();
        let mut lexer = Lexer::new(src);

        assert!(matches!(lexer.get_next_token(), Token::Operator { .. }));
        assert!(matches!(lexer.get_next_token(), Token::Eof));
    }
}

fn main() {
    let s = "var i, s;
    begin
        i := 0; s := 0;
        while i < 5 do
        begin
            i := i + 1;
            s := s + i * i
        end
    end."
        .into();

    let lexer = Lexer::new(s);
    let mut parser = Parser::new(lexer);

    println!("{:?}", parser.parse());
}
