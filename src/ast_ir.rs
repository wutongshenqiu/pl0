use crate::{
    ASTNode, BlockASTNode, CondASTNode, ExprASTNode, FactorASTNode, OperatorToken, Pl0Error,
    Result, StmtASTNode, TermASTNode,
};

#[derive(Debug, Clone)]
pub enum IrOpCode {
    Add,
    Sub,
    Neg,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Odd,
    LoadVar,
    LoadLit,
    Store,
    Jump,
    BrFalse,
    DefVar,
    DefLit,
    DefProc,
    Call,
    Input,
    Output,
    Halt,
}

#[derive(Debug, Clone)]
pub enum IrArg {
    Ident(String),
    Number(i64),
}

impl From<&i64> for IrArg {
    fn from(value: &i64) -> Self {
        Self::Number(*value)
    }
}

impl From<i64> for IrArg {
    fn from(value: i64) -> Self {
        Self::Number(value)
    }
}

impl From<&str> for IrArg {
    fn from(value: &str) -> Self {
        Self::Ident(value.into())
    }
}

impl From<String> for IrArg {
    fn from(value: String) -> Self {
        Self::Ident(value)
    }
}

#[derive(Debug, Clone)]
pub enum IrValue {
    Number(i64),
    Irs(Vec<Ir>),
}

impl From<&i64> for IrValue {
    fn from(value: &i64) -> Self {
        Self::Number(*value)
    }
}

impl From<i64> for IrValue {
    fn from(value: i64) -> Self {
        Self::Number(value)
    }
}

impl From<Vec<Ir>> for IrValue {
    fn from(value: Vec<Ir>) -> Self {
        Self::Irs(value)
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub op: IrOpCode,
    pub arg: Option<IrArg>,
    pub value: Option<IrValue>,
}

impl Default for Ir {
    fn default() -> Self {
        Self {
            op: IrOpCode::Add,
            arg: None,
            value: None,
        }
    }
}

pub trait ASTNodeGen {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()>;
}

impl ASTNodeGen for ASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        match self {
            Self::Block(Some(block)) => block.gen(buf)?,
            Self::Stmt(Some(stmt)) => stmt.gen(buf)?,
            Self::Cond(Some(cond)) => cond.gen(buf)?,
            Self::Expr(Some(expr)) => expr.gen(buf)?,
            Self::Term(Some(term)) => term.gen(buf)?,
            Self::Factor(Some(factor)) => factor.gen(buf)?,
            _ => return Err(Pl0Error::EmptyASTNode),
        }

        Ok(())
    }
}

impl ASTNodeGen for BlockASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        for (name, value) in self.consts.iter() {
            buf.push(Ir {
                op: IrOpCode::DefLit,
                arg: Some(name.as_str().into()),
                value: Some(value.into()),
            });
        }

        for name in self.vars.iter() {
            buf.push(Ir {
                op: IrOpCode::DefVar,
                arg: Some(name.as_str().into()),
                value: None,
            });
        }

        for (name, pp) in self.procedures.iter() {
            let mut proc_buf = Vec::new();
            pp.gen(&mut proc_buf)?;
            buf.push(Ir {
                op: IrOpCode::DefProc,
                arg: Some(name.as_str().into()),
                value: Some(proc_buf.into()),
            });
        }

        self.stmt.gen(buf)?;

        buf.push(Ir {
            op: IrOpCode::Halt,
            ..Default::default()
        });
        Ok(())
    }
}

impl ASTNodeGen for StmtASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        match self {
            Self::Assign(name, expr) => {
                expr.gen(buf)?;
                buf.push(Ir {
                    op: IrOpCode::Store,
                    arg: Some(name.as_str().into()),
                    value: None,
                });
            }
            Self::Call(name) => {
                buf.push(Ir {
                    op: IrOpCode::Call,
                    arg: Some(name.as_str().into()),
                    value: None,
                });
            }
            Self::Input(name) => {
                buf.push(Ir {
                    op: IrOpCode::Input,
                    ..Default::default()
                });
                buf.push(Ir {
                    op: IrOpCode::Store,
                    arg: Some(name.as_str().into()),
                    value: None,
                });
            }
            Self::Output(expr) => {
                expr.gen(buf)?;
                buf.push(Ir {
                    op: IrOpCode::Output,
                    ..Default::default()
                });
            }
            Self::Begin(stmts) => {
                for stmt in stmts.iter() {
                    stmt.gen(buf)?;
                }
            }
            Self::If(cond, box stmt) => {
                cond.gen(buf)?;
                let i = buf.len();
                buf.push(Ir {
                    op: IrOpCode::BrFalse,
                    ..Default::default()
                });
                stmt.gen(buf)?;
                buf[i] = Ir {
                    op: IrOpCode::BrFalse,
                    arg: Some((buf.len() as i64).into()),
                    value: None,
                };
            }
            Self::While(cond, box stmt) => {
                let i = buf.len();
                cond.gen(buf)?;
                let j = buf.len();
                buf.push(Ir {
                    op: IrOpCode::BrFalse,
                    ..Default::default()
                });
                stmt.gen(buf)?;
                buf.push(Ir {
                    op: IrOpCode::Jump,
                    arg: Some((i as i64).into()),
                    value: None,
                });
                buf[j] = Ir {
                    op: IrOpCode::BrFalse,
                    arg: Some((buf.len() as i64).into()),
                    value: None,
                };
            }
        }

        Ok(())
    }
}

impl ASTNodeGen for CondASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        match self {
            Self::OddCond(expr) => {
                expr.gen(buf)?;
                buf.push(Ir {
                    op: IrOpCode::Odd,
                    ..Default::default()
                });
            }
            Self::StdCond(expr_l, op, expr_r) => {
                expr_l.gen(buf)?;
                expr_r.gen(buf)?;

                match op {
                    OperatorToken::Equal => buf.push(Ir {
                        op: IrOpCode::Eq,
                        ..Default::default()
                    }),
                    OperatorToken::Hashtag => buf.push(Ir {
                        op: IrOpCode::Ne,
                        ..Default::default()
                    }),
                    OperatorToken::Greater => buf.push(Ir {
                        op: IrOpCode::Gt,
                        ..Default::default()
                    }),
                    OperatorToken::GEqual => buf.push(Ir {
                        op: IrOpCode::Gte,
                        ..Default::default()
                    }),
                    OperatorToken::Less => buf.push(Ir {
                        op: IrOpCode::Lt,
                        ..Default::default()
                    }),
                    OperatorToken::LEqual => buf.push(Ir {
                        op: IrOpCode::Lte,
                        ..Default::default()
                    }),
                    _ => return Err(Pl0Error::InvalidStdCondOp),
                }
            }
        }

        Ok(())
    }
}

impl ASTNodeGen for ExprASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        // TODO(optimize): add 0 at first to keep consistency, but may produce useless ir
        // buf.push(Ir { op: IrOpCode::LoadLit, arg: Some(0.into()), value: None });

        // for (op, term) in self.iter() {
        //     term.gen(buf)?;
        //     match op {
        //         OperatorToken::Plus => buf.push(Ir { op: IrOpCode::Add, ..Default::default() }),
        //         OperatorToken::Minus => buf.push(Ir { op: IrOpCode::Sub, ..Default::default() }),
        //         _ => return Err(Pl0Error::InvalidExprOp)
        //     }
        // }

        // TODO(refactor): a little ugly
        let (op, lhs) = &self[0];
        lhs.gen(buf)?;
        match op {
            OperatorToken::Minus => buf.push(Ir {
                op: IrOpCode::Neg,
                ..Default::default()
            }),
            OperatorToken::Plus => (),
            _ => return Err(Pl0Error::InvalidExprOp),
        }

        for (op, rhs) in self.iter().skip(1) {
            rhs.gen(buf)?;
            match op {
                OperatorToken::Minus => buf.push(Ir {
                    op: IrOpCode::Sub,
                    ..Default::default()
                }),
                OperatorToken::Plus => buf.push(Ir {
                    op: IrOpCode::Add,
                    ..Default::default()
                }),
                _ => return Err(Pl0Error::InvalidExprOp),
            }
        }

        Ok(())
    }
}

impl ASTNodeGen for TermASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        self.lhs.gen(buf)?;

        for (op, rhs) in self.rhs.iter() {
            rhs.gen(buf)?;
            match op {
                OperatorToken::Mul => buf.push(Ir {
                    op: IrOpCode::Mul,
                    ..Default::default()
                }),
                OperatorToken::Div => buf.push(Ir {
                    op: IrOpCode::Div,
                    ..Default::default()
                }),
                _ => return Err(Pl0Error::InvalidTermOp),
            }
        }

        Ok(())
    }
}

impl ASTNodeGen for FactorASTNode {
    fn gen(&self, buf: &mut Vec<Ir>) -> Result<()> {
        match self {
            Self::Ident(name) => buf.push(Ir {
                op: IrOpCode::LoadVar,
                arg: Some(name.as_str().into()),
                value: None,
            }),
            Self::Number(number) => buf.push(Ir {
                op: IrOpCode::LoadLit,
                arg: Some(number.into()),
                value: None,
            }),
            Self::Expr(expr) => expr.gen(buf)?,
        }

        Ok(())
    }
}
