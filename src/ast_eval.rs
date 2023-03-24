use std::io;

use crate::{
    ASTNode, BlockASTNode, CondASTNode, Context, ExprASTNode, FactorASTNode, OperatorToken,
    Pl0Error, Result, StmtASTNode, TermASTNode,
};

type ContextBlockAST = Context<BlockASTNode>;

pub trait ASTNodeEval {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>>;
}

impl ASTNodeEval for FactorASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        match self {
            Self::Ident(name) => Ok(Some(context.get_value(name)?)),
            Self::Number(number) => Ok(Some(number).cloned()),
            Self::Expr(expr) => expr.eval(context),
        }
    }
}

impl ASTNodeEval for TermASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        let mut ret = self.lhs.eval(context)?.ok_or(Pl0Error::InvalidTerm)?;

        for (op, rhs) in self.rhs.iter() {
            let val = rhs.eval(context)?.ok_or(Pl0Error::InvalidTerm)?;
            match op {
                OperatorToken::Mul => {
                    ret *= val;
                }
                OperatorToken::Div => {
                    if val == 0 {
                        return Err(Pl0Error::DivisionByZero);
                    }
                    ret /= val;
                }
                _ => return Err(Pl0Error::InvalidTermOp),
            }
        }

        Ok(Some(ret))
    }
}

impl ASTNodeEval for ExprASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        let mut ret = 0;

        for (op, term) in self.iter() {
            let val = term.eval(context)?.ok_or(Pl0Error::InvalidExpr)?;

            match op {
                OperatorToken::Plus => ret += val,
                OperatorToken::Minus => ret -= val,
                _ => return Err(Pl0Error::InvalidExprOp),
            }
        }

        Ok(Some(ret))
    }
}

impl ASTNodeEval for CondASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        match self {
            Self::OddCond(expr) => {
                let ret = expr.eval(context)?.ok_or(Pl0Error::InvalidOddCond)?;
                // 返回 1 代表 true, 0 代表 false
                Ok(Some(ret & 1))
            }
            Self::StdCond(expr_l, op, expr_r) => {
                // 没有 and/or，因此不需要考虑短路运算
                let ret_l = expr_l.eval(context)?.ok_or(Pl0Error::InvalidStdCond)?;
                let ret_r = expr_r.eval(context)?.ok_or(Pl0Error::InvalidStdCond)?;

                match op {
                    OperatorToken::Equal => Ok(Some((ret_l == ret_r).into())),
                    OperatorToken::Hashtag => Ok(Some((ret_l != ret_r).into())),
                    OperatorToken::Greater => Ok(Some((ret_l > ret_r).into())),
                    OperatorToken::GEqual => Ok(Some((ret_l >= ret_r).into())),
                    OperatorToken::Less => Ok(Some((ret_l < ret_r).into())),
                    OperatorToken::LEqual => Ok(Some((ret_l <= ret_r).into())),
                    _ => Err(Pl0Error::InvalidStdCondOp),
                }
            }
        }
    }
}

impl ASTNodeEval for StmtASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        match self {
            Self::Assign(name, expr) => {
                let val = expr.eval(context)?.ok_or(Pl0Error::InvalidAssign)?;
                context.update_var_value(name, val)?;
            }
            Self::Call(name) => {
                // TODO(optimize): 使用引用而不是拷贝
                // =========== 问题分析 =======================
                // let proc = context.get_proc(name)?;  // context 的不可变引用
                // proc.eval(context)?;                 // context 的可变引用
                // context 会构造一个新的栈帧, 即 proc.eval(context)?; 的可变借用不会修改可变引用的部分
                // ===========================================
                let proc = context.get_proc(name).cloned()?;
                context.new_frame();
                proc.eval(context)?;
                context.pop_frame()?;
            }
            Self::Input(name) => {
                let mut line = String::new();
                io::stdin().read_line(&mut line)?;
                let value: i64 = line.trim().parse()?;
                context.update_var_value(name, value)?;
            }
            Self::Output(expr) => {
                let value = expr.eval(context)?.ok_or(Pl0Error::InvalidOutput)?;
                println!("{}", value);
            }
            Self::Begin(stmts) => {
                for stmt in stmts.iter() {
                    stmt.eval(context)?;
                }
            }
            Self::If(cond, box stmt) => {
                if let 1 = cond.eval(context)?.ok_or(Pl0Error::InvalidIf)? {
                    stmt.eval(context)?;
                }
            }
            Self::While(cond, box stmt) => {
                while let 1 = cond.eval(context)?.ok_or(Pl0Error::InvalidWhile)? {
                    stmt.eval(context)?;
                }
            }
        };
        Ok(None)
    }
}

impl ASTNodeEval for BlockASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        for (name, value) in self.consts.iter() {
            context.insert_const(name, *value)?;
        }

        for name in self.vars.iter() {
            context.add_var(name)?;
        }

        for (name, stmt) in self.procedures.iter() {
            // TODO(optimize): 引用
            context.insert_proc(name, stmt.clone())?;
        }

        self.stmt.eval(context)
    }
}

impl ASTNodeEval for ASTNode {
    fn eval(&self, context: &mut ContextBlockAST) -> Result<Option<i64>> {
        match self {
            Self::Block(Some(block)) => block.eval(context),
            Self::Stmt(Some(stmt)) => stmt.eval(context),
            Self::Cond(Some(cond)) => cond.eval(context),
            Self::Expr(Some(expr)) => expr.eval(context),
            Self::Term(Some(term)) => term.eval(context),
            Self::Factor(Some(factor)) => factor.eval(context),
            _ => Err(Pl0Error::EmptyASTNode),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::Parser;

    #[test]
    fn test_expr_eval() {
        let mut context = Context::new();

        let mut expr = ExprASTNode::new();
        expr.push((
            OperatorToken::Plus,
            TermASTNode {
                lhs: FactorASTNode::Number(1),
                rhs: vec![],
            },
        ));
        assert_eq!(expr.eval(&mut context).unwrap(), Some(1));

        expr.push((
            OperatorToken::Minus,
            TermASTNode {
                lhs: FactorASTNode::Number(10),
                rhs: vec![],
            },
        ));
        assert_eq!(expr.eval(&mut context).unwrap(), Some(-9));

        expr.push((
            OperatorToken::Mul,
            TermASTNode {
                lhs: FactorASTNode::Number(1),
                rhs: vec![],
            },
        ));
        assert!(matches!(
            expr.eval(&mut context),
            Err(Pl0Error::InvalidExprOp)
        ));
    }

    #[test]
    fn test_term_eval() {
        let mut context = Context::new();

        let term = TermASTNode {
            lhs: FactorASTNode::Number(2),
            rhs: vec![
                (OperatorToken::Mul, FactorASTNode::Number(3)),
                (OperatorToken::Div, FactorASTNode::Number(2)),
            ],
        };
        assert_eq!(term.eval(&mut context).unwrap(), Some(3));
    }

    #[test]
    fn test_factor_eval() {
        let mut context = Context::new();

        let factor = FactorASTNode::Number(1);
        assert_eq!(factor.eval(&mut context).unwrap(), Some(1));

        let factor = FactorASTNode::Ident("a".into());
        assert!(matches!(
            factor.eval(&mut context),
            Err(Pl0Error::UndefinedSymbol(_))
        ));

        context.insert_var("a", Some(1)).unwrap();
        assert_eq!(factor.eval(&mut context).unwrap(), Some(1));
    }

    #[test]
    fn test_odd_cond() {
        let mut context = Context::new();

        let expr = make_simple_expr(vec![1]);
        let odd_cond = CondASTNode::OddCond(expr);
        assert_eq!(odd_cond.eval(&mut context).unwrap(), Some(1));

        let expr = make_simple_expr(vec![1, 2, 3]);
        let odd_cond = CondASTNode::OddCond(expr);
        assert_eq!(odd_cond.eval(&mut context).unwrap(), Some(0));
    }

    #[test]
    fn test_std_cond() {
        let mut context = Context::new();

        let std_eq = make_simple_std_cond(vec![1], OperatorToken::Equal, vec![1]);
        assert_eq!(std_eq.eval(&mut context).unwrap(), Some(1));
        let std_eq = make_simple_std_cond(vec![1], OperatorToken::Equal, vec![2]);
        assert_eq!(std_eq.eval(&mut context).unwrap(), Some(0));

        let std_ne = make_simple_std_cond(vec![1], OperatorToken::Hashtag, vec![1]);
        assert_eq!(std_ne.eval(&mut context).unwrap(), Some(0));
        let std_ne = make_simple_std_cond(vec![1], OperatorToken::Hashtag, vec![2]);
        assert_eq!(std_ne.eval(&mut context).unwrap(), Some(1));

        let std_g = make_simple_std_cond(vec![1], OperatorToken::Greater, vec![2]);
        assert_eq!(std_g.eval(&mut context).unwrap(), Some(0));
        let std_g = make_simple_std_cond(vec![2], OperatorToken::Greater, vec![1]);
        assert_eq!(std_g.eval(&mut context).unwrap(), Some(1));

        let std_ge = make_simple_std_cond(vec![2], OperatorToken::GEqual, vec![2]);
        assert_eq!(std_ge.eval(&mut context).unwrap(), Some(1));
        let std_ge = make_simple_std_cond(vec![2], OperatorToken::GEqual, vec![1]);
        assert_eq!(std_ge.eval(&mut context).unwrap(), Some(1));
        let std_ge = make_simple_std_cond(vec![2], OperatorToken::GEqual, vec![3]);
        assert_eq!(std_ge.eval(&mut context).unwrap(), Some(0));

        let std_l = make_simple_std_cond(vec![1], OperatorToken::Less, vec![2]);
        assert_eq!(std_l.eval(&mut context).unwrap(), Some(1));
        let std_l = make_simple_std_cond(vec![2], OperatorToken::Less, vec![1]);
        assert_eq!(std_l.eval(&mut context).unwrap(), Some(0));

        let std_le = make_simple_std_cond(vec![2], OperatorToken::LEqual, vec![2]);
        assert_eq!(std_le.eval(&mut context).unwrap(), Some(1));
        let std_le = make_simple_std_cond(vec![2], OperatorToken::LEqual, vec![1]);
        assert_eq!(std_le.eval(&mut context).unwrap(), Some(0));
        let std_le = make_simple_std_cond(vec![2], OperatorToken::LEqual, vec![3]);
        assert_eq!(std_le.eval(&mut context).unwrap(), Some(1));

        let std_wrong = make_simple_std_cond(vec![1], OperatorToken::Div, vec![2]);
        assert!(matches!(
            std_wrong.eval(&mut context),
            Err(Pl0Error::InvalidStdCondOp)
        ));
    }

    #[test]
    fn test_stmt_assign() {
        let mut context = Context::new();

        let stmt_assign = make_simple_assign_stmt("a", 1);
        assert!(matches!(
            stmt_assign.eval(&mut context),
            Err(Pl0Error::UndefinedSymbol(_))
        ));
        context.add_var("a").unwrap();
        assert_eq!(stmt_assign.eval(&mut context).unwrap(), None);
        assert_eq!(context.get_value("a").unwrap(), 1);
    }

    #[test]
    fn test_stmt_if_true() {
        let mut context = Context::new();

        context.add_var("a").unwrap();
        let cond = make_simple_std_cond(vec![1], OperatorToken::Greater, vec![0]);
        let stmt_if = StmtASTNode::If(cond, Box::new(make_simple_assign_stmt("a", 1)));
        stmt_if.eval(&mut context).unwrap();
        assert!(matches!(context.get_value("a").unwrap(), 1));
    }

    #[test]
    fn test_stmt_if_false() {
        let mut context = Context::new();

        context.add_var("a").unwrap();
        let cond = make_simple_std_cond(vec![0], OperatorToken::Greater, vec![1]);
        let stmt_if = StmtASTNode::If(cond, Box::new(make_simple_assign_stmt("a", 1)));
        stmt_if.eval(&mut context).unwrap();
        assert!(matches!(
            context.get_value("a"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));
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
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("i").unwrap(), 5);
        assert_eq!(context.get_value("s").unwrap(), 55);
    }

    #[test]
    fn test_proc_basic() {
        let src = "
        const a = 1;
        procedure P;
            var b;
            b := 1;
        call P
        .";
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("a").unwrap(), 1);
        assert!(matches!(
            context.get_value("b"),
            Err(Pl0Error::UndefinedSymbol(_))
        ));
        assert!(matches!(
            context.get_proc("P").unwrap(),
            BlockASTNode { .. }
        ));
    }

    #[test]
    fn test_proc_local() {
        let src = "
        const a = 1;
        var b;
        procedure P;
            const a = 2;
            var b;
            b := 1;
        begin
            b := 2;
            call P
        end
        .
        ";
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("a").unwrap(), 1);
        assert_eq!(context.get_value("b").unwrap(), 2);
    }

    #[test]
    fn test_proc_overwrite_global() {
        let src = "
        var a;
        procedure P;
            a := 2;
        begin
            a := 1;
            call P
        end
        .";
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("a").unwrap(), 2);
    }

    #[test]
    fn test_nested_proc() {
        let src = "
        var a;
        procedure PO;
            procedure PI;
                a := a + 1;
            begin
                call PI;
                a := a * 2
            end;
        begin
            a := 1;
            call PO
        end
        .
        ";
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("a").unwrap(), 4);
    }

    #[test]
    fn test_recursive_proc() {
        let src = "
        var n, ans;
        
        procedure fact;
            if n > 1 then 
            begin
                ans := ans * n;
                n := n - 1;
                call fact
            end;
        
        begin 
            n := 5;
            ans := 1;
            call fact
        end
        .
        ";
        let ast = Parser::parse_str(src).unwrap();
        let mut context = Context::new();
        ast.eval(&mut context).unwrap();

        assert_eq!(context.get_value("ans").unwrap(), 120);
    }

    fn make_simple_expr(nums: Vec<i64>) -> ExprASTNode {
        let mut expr = ExprASTNode::new();
        for num in nums.into_iter() {
            expr.push((
                OperatorToken::Plus,
                TermASTNode {
                    lhs: FactorASTNode::Number(num),
                    rhs: vec![],
                },
            ));
        }
        expr
    }

    fn make_simple_std_cond(lnums: Vec<i64>, op: OperatorToken, rnums: Vec<i64>) -> CondASTNode {
        CondASTNode::StdCond(make_simple_expr(lnums), op, make_simple_expr(rnums))
    }

    fn make_simple_assign_stmt(name: &str, value: i64) -> StmtASTNode {
        StmtASTNode::Assign(name.into(), make_simple_expr(vec![value]))
    }

    pub fn make_simple_assign_block(name: &str, value: i64) -> BlockASTNode {
        BlockASTNode {
            consts: vec![],
            vars: vec![name.into()],
            procedures: vec![],
            stmt: make_simple_assign_stmt(name, value),
        }
    }
}
