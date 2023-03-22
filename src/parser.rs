use crate::{KeywordToken, Lexer, OperatorToken, Pl0Error, Result, Token};

#[derive(Debug)]
struct BlockASTNode {
    consts: Vec<(String, i64)>,
    vars: Vec<String>,
    procedures: Vec<(String, BlockASTNode)>,
    stmt: StmtASTNode,
}

#[derive(Debug)]
enum StmtASTNode {
    Assign(String, ExprASTNode),
    Call(String),
    Begin(Vec<StmtASTNode>),
    If(CondASTNode, Box<StmtASTNode>),
    While(CondASTNode, Box<StmtASTNode>),
}

#[derive(Debug)]
enum CondASTNode {
    OddCond(ExprASTNode),
    StdCond(ExprASTNode, OperatorToken, ExprASTNode),
}

type ExprASTNode = Vec<(OperatorToken, TermASTNode)>;

#[derive(Debug)]
struct TermASTNode {
    lhs: FactorASTNode,
    rhs: Vec<(OperatorToken, FactorASTNode)>,
}

#[derive(Debug)]
enum FactorASTNode {
    Ident(String),
    Number(i64),
    Expr(ExprASTNode),
}

#[derive(Debug)]
enum ASTNode {
    Block(Option<BlockASTNode>),
    Stmt(Option<StmtASTNode>),
    Cond(Option<CondASTNode>),
    Expr(Option<ExprASTNode>),
    Term(Option<TermASTNode>),
    Factor(Option<FactorASTNode>),
}

struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse(mut self) -> Result<ASTNode> {
        self.program()
    }
}

impl Parser {
    /// Consume next token from lexer and check whether it is expected
    fn expect(&mut self, token: Token) -> Result<()> {
        let tk = self.lexer.get_next_token()?;

        if tk != token {
            Err(Pl0Error::UnexpectedToken {
                token: tk,
                expected_token: token,
            })
        } else {
            Ok(())
        }
    }

    fn next_ident(&mut self) -> Result<String> {
        let tk = self.lexer.get_next_token()?;

        match tk {
            Token::Ident(name) => Ok(name),
            x => Err(Pl0Error::InvalidToken(x)),
        }
    }

    fn next_number(&mut self) -> Result<i64> {
        let tk = self.lexer.get_next_token()?;

        match tk {
            Token::Number(x) => Ok(x),
            x => Err(Pl0Error::InvalidToken(x)),
        }
    }

    fn program(&mut self) -> Result<ASTNode> {
        let block = self.block()?;
        self.expect(".".into())?;

        Ok(ASTNode::Block(Some(block)))
    }

    fn block(&mut self) -> Result<BlockASTNode> {
        let mut consts = Vec::new();
        let mut vars = Vec::new();
        let mut procedures = Vec::new();

        if let Token::Keyword(KeywordToken::Const) = self.lexer.peek_next_token()? {
            self.expect("const".into())?;
            let ident = self.next_ident()?;
            self.expect("=".into())?;
            let number = self.next_number()?;
            consts.push((ident, number));

            loop {
                match self.lexer.get_next_token()? {
                    Token::Operator(op) => match op {
                        OperatorToken::Semi => break,
                        OperatorToken::Comma => {
                            let ident = self.next_ident()?;
                            self.expect("=".into())?;
                            let number = self.next_number()?;
                            consts.push((ident, number));
                        }
                        x => return Err(Pl0Error::InvalidToken(Token::Operator(x))),
                    },
                    x => return Err(Pl0Error::InvalidToken(x)),
                }
            }
        }

        if let Token::Keyword(KeywordToken::Var) = self.lexer.peek_next_token()? {
            self.expect("var".into())?;
            let ident = self.next_ident()?;
            vars.push(ident);

            loop {
                match self.lexer.get_next_token()? {
                    Token::Operator(op) => match op {
                        OperatorToken::Semi => break,
                        OperatorToken::Comma => {
                            let ident = self.next_ident()?;
                            vars.push(ident);
                        }
                        x => return Err(Pl0Error::InvalidToken(Token::Operator(x))),
                    },
                    x => return Err(Pl0Error::InvalidToken(x)),
                }
            }
        }

        loop {
            match self.lexer.peek_next_token()? {
                Token::Keyword(kw) if matches!(kw, KeywordToken::Procedure) => {
                    self.expect("procedure".into())?;
                    let ident = self.next_ident()?;
                    self.expect(";".into())?;
                    let block = self.block()?;
                    procedures.push((ident, block));
                    self.expect(";".into())?;
                }
                x => break,
            }
        }

        let stmt = self.stmt()?;

        Ok(BlockASTNode {
            consts,
            vars,
            procedures,
            stmt,
        })
    }

    fn stmt(&mut self) -> Result<StmtASTNode> {
        match self.lexer.get_next_token()? {
            Token::Ident(name) => {
                self.expect(":=".into())?;
                Ok(StmtASTNode::Assign(name, self.expr()?))
            }
            Token::Keyword(kw) => match kw {
                KeywordToken::Call => {
                    let ident = self.next_ident()?;
                    Ok(StmtASTNode::Call(ident))
                }
                KeywordToken::Begin => {
                    let mut stmts = Vec::new();
                    stmts.push(self.stmt()?);

                    loop {
                        match self.lexer.peek_next_token()? {
                            Token::Operator(op) if matches!(op, OperatorToken::Semi) => {
                                self.expect(";".into())?;
                                stmts.push(self.stmt()?);
                            }
                            _ => break,
                        }
                    }
                    self.expect("end".into())?;
                    Ok(StmtASTNode::Begin(stmts))
                }
                KeywordToken::If => {
                    let cond = self.cond()?;
                    self.expect("then".into())?;
                    let stmt = self.stmt()?;
                    Ok(StmtASTNode::If(cond, Box::new(stmt)))
                }
                KeywordToken::While => {
                    let cond = self.cond()?;
                    self.expect("do".into())?;
                    let stmt = self.stmt()?;

                    Ok(StmtASTNode::While(cond, Box::new(stmt)))
                }
                x => Err(Pl0Error::InvalidToken(Token::Keyword(x))),
            },
            x => Err(Pl0Error::InvalidToken(x)),
        }
    }

    fn cond(&mut self) -> Result<CondASTNode> {
        if let Token::Keyword(kw) = self.lexer.peek_next_token()? {
            self.expect("odd".into())?;
            let expr = self.expr()?;
            Ok(CondASTNode::OddCond(expr))
        } else {
            let expr_l = self.expr()?;
            match self.lexer.get_next_token()? {
                Token::Operator(op) => match op {
                    OperatorToken::Equal
                    | OperatorToken::Hashtag
                    | OperatorToken::Greater
                    | OperatorToken::Less
                    | OperatorToken::GEqual
                    | OperatorToken::LEqual => {
                        let expr_r = self.expr()?;
                        Ok(CondASTNode::StdCond(expr_l, op, expr_r))
                    }
                    _ => Err(Pl0Error::InvalidToken(Token::Operator(op))),
                },
                x => Err(Pl0Error::InvalidToken(x)),
            }
        }
    }

    fn expr(&mut self) -> Result<ExprASTNode> {
        let mut v = Vec::new();

        let op = match self.lexer.peek_next_token()? {
            Token::Operator(op) if matches!(op, OperatorToken::Minus) => op,
            _ => OperatorToken::Plus,
        };
        v.push((op, self.term()?));

        loop {
            match self.lexer.peek_next_token()? {
                Token::Operator(op)
                    if matches!(op, OperatorToken::Plus) || matches!(op, OperatorToken::Minus) =>
                {
                    self.expect(Token::Operator(op))?;
                    v.push((op, self.term()?));
                }
                _ => break,
            }
        }

        Ok(v)
    }

    fn term(&mut self) -> Result<TermASTNode> {
        let lhs = self.factor()?;
        let mut rhs = Vec::new();

        loop {
            match self.lexer.peek_next_token()? {
                Token::Operator(op)
                    if matches!(op, OperatorToken::Mul) || matches!(op, OperatorToken::Div) =>
                {
                    self.expect(Token::Operator(op))?;
                    rhs.push((op, self.factor()?));
                }
                _ => break,
            }
        }

        Ok(TermASTNode { lhs, rhs })
    }

    fn factor(&mut self) -> Result<FactorASTNode> {
        match self.lexer.get_next_token()? {
            Token::Ident(name) => Ok(FactorASTNode::Ident(name)),
            Token::Number(value) => Ok(FactorASTNode::Number(value)),
            Token::Operator(op) => {
                if !matches!(op, OperatorToken::LPar) {
                    return Err(Pl0Error::InvalidToken(Token::Operator(op)));
                }

                let expr = self.expr()?;
                self.expect(")".into())?;

                Ok(FactorASTNode::Expr(expr))
            }
            x => Err(Pl0Error::InvalidToken(x)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_procedure() {
        let src = "
        procedure P1;
        a := 1;
        b := 1
        .";
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);

        let ast = parser.parse().unwrap();

        match ast {
            ASTNode::Block(Some(block)) => {
                let BlockASTNode {
                    consts,
                    vars,
                    procedures,
                    stmt,
                } = block;

                assert_eq!(consts.len(), 0);
                assert_eq!(vars.len(), 0);
                assert_eq!(procedures.len(), 1);
                assert_eq!(procedures[0].0, "P1");
            }
            _ => panic!("expect block"),
        }
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);

        let ast = parser.parse().unwrap();

        match ast {
            ASTNode::Block(Some(block)) => {
                let BlockASTNode {
                    consts,
                    vars,
                    procedures,
                    stmt,
                } = block;

                assert_eq!(consts.len(), 0);
                assert_eq!(vars, vec!["i", "s"]);
                assert_eq!(procedures.len(), 0);

                assert!(matches!(stmt, StmtASTNode::Begin(_)));
            }
            _ => panic!("expect block"),
        }
    }
}
