use std::{collections::HashMap, io};

use crate::{
    ASTNode, BlockASTNode, CondASTNode, ExprASTNode, FactorASTNode, OperatorToken, Pl0Error,
    Result, StmtASTNode, TermASTNode,
};

#[derive(Debug)]
struct EvalFrame {
    vars: HashMap<String, Option<i64>>,
    consts: HashMap<String, i64>,
    // TODO(refactor): 使用引用而不是拷贝
    // 如何确保 ASTNode 的生命周期大于 EvalFrame?
    procs: HashMap<String, BlockASTNode>,
}

impl EvalFrame {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            consts: HashMap::new(),
            procs: HashMap::new(),
        }
    }

    pub fn get_value(&self, name: &str) -> Result<i64> {
        match self.vars.get(name) {
            Some(value_opt) => value_opt.ok_or(Pl0Error::VarUsedBeforeInitialize(name.into())),
            None => self
                .consts
                .get(name)
                .cloned()
                .ok_or(Pl0Error::UndefinedSymbol(name.into())),
        }
    }

    pub fn get_proc(&self, name: &str) -> Result<&BlockASTNode> {
        self.procs
            .get(name)
            .ok_or(Pl0Error::UndefinedSymbol(name.into()))
    }

    pub fn check_symbol_exist(&self, name: &str) -> bool {
        self.vars.contains_key(name)
            || self.consts.contains_key(name)
            || self.procs.contains_key(name)
    }

    pub fn add_var(&mut self, name: &str) -> Result<()> {
        self.insert_var(name, None)
    }

    pub fn update_var_value(&mut self, name: &str, value: i64) -> Result<()> {
        if !self.vars.contains_key(name) {
            Err(Pl0Error::UndefinedSymbol(name.into()))
        } else {
            self.vars.insert(name.into(), Some(value));
            Ok(())
        }
    }

    pub fn insert_var(&mut self, name: &str, value: Option<i64>) -> Result<()> {
        if self.check_symbol_exist(name) {
            Err(Pl0Error::RedefinedSymbol(name.into()))
        } else {
            self.vars.insert(name.into(), value);
            Ok(())
        }
    }

    pub fn insert_const(&mut self, name: &str, value: i64) -> Result<()> {
        if self.check_symbol_exist(name) {
            Err(Pl0Error::RedefinedSymbol(name.into()))
        } else {
            self.consts.insert(name.into(), value);
            Ok(())
        }
    }

    pub fn insert_proc(&mut self, name: &str, stmt: BlockASTNode) -> Result<()> {
        if self.check_symbol_exist(name) {
            Err(Pl0Error::RedefinedSymbol(name.into()))
        } else {
            self.procs.insert(name.into(), stmt);
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct EvalContext {
    st: Vec<EvalFrame>,
}

impl EvalContext {
    pub fn new() -> Self {
        Self {
            st: vec![EvalFrame::new()],
        }
    }

    pub fn get_value(&self, name: &str) -> Result<i64> {
        for frame in self.st.iter().rev() {
            match frame.get_value(name) {
                Ok(value) => return Ok(value),
                Err(Pl0Error::VarUsedBeforeInitialize(x)) => {
                    return Err(Pl0Error::VarUsedBeforeInitialize(x))
                }
                _ => (),
            }
        }

        Err(Pl0Error::UndefinedSymbol(name.into()))
    }

    pub fn get_proc(&self, name: &str) -> Result<&BlockASTNode> {
        for frame in self.st.iter().rev() {
            if let Ok(proc) = frame.get_proc(name) {
                return Ok(proc);
            }
        }

        Err(Pl0Error::UndefinedSymbol(name.into()))
    }

    pub fn check_symbol_exist(&self, name: &str) -> bool {
        self.st.iter().rev().any(|x| x.check_symbol_exist(name))
    }

    pub fn add_var(&mut self, name: &str) -> Result<()> {
        self.st
            .last_mut()
            .ok_or(Pl0Error::EmptyStackFrame)?
            .add_var(name)
    }

    pub fn update_var_value(&mut self, name: &str, value: i64) -> Result<()> {
        for frame in self.st.iter_mut().rev() {
            if let Ok(()) = frame.update_var_value(name, value) {
                return Ok(());
            }
        }

        Err(Pl0Error::UndefinedSymbol(name.into()))
    }

    pub fn insert_var(&mut self, name: &str, value: Option<i64>) -> Result<()> {
        self.st
            .last_mut()
            .ok_or(Pl0Error::EmptyStackFrame)?
            .insert_var(name, value)
    }

    pub fn insert_const(&mut self, name: &str, value: i64) -> Result<()> {
        self.st
            .last_mut()
            .ok_or(Pl0Error::EmptyStackFrame)?
            .insert_const(name, value)
    }

    pub fn insert_proc(&mut self, name: &str, stmt: BlockASTNode) -> Result<()> {
        self.st
            .last_mut()
            .ok_or(Pl0Error::EmptyStackFrame)?
            .insert_proc(name, stmt)
    }

    pub fn new_frame(&mut self) {
        self.st.push(EvalFrame::new());
    }

    pub fn pop_frame(&mut self) -> Result<()> {
        self.st.pop().ok_or(Pl0Error::EmptyStackFrame)?;

        Ok(())
    }

    pub fn depth(&self) -> usize {
        self.st.len()
    }
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}

pub trait ASTNodeEval {
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>>;
}

impl ASTNodeEval for FactorASTNode {
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
        match self {
            Self::Ident(name) => Ok(Some(context.get_value(name)?)),
            Self::Number(number) => Ok(Some(number).cloned()),
            Self::Expr(expr) => expr.eval(context),
        }
    }
}

impl ASTNodeEval for TermASTNode {
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
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
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
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
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
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
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
        match self {
            Self::Assign(name, expr) => {
                let val = expr.eval(context)?.ok_or(Pl0Error::InvalidAssign)?;
                context.update_var_value(name, val)?;
            }
            Self::Call(name) => {
                // TODO(refactor) 使用引用而不是拷贝
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
    fn eval<'a>(&'a self, context: &'a mut EvalContext) -> Result<Option<i64>> {
        for (name, value) in self.consts.iter() {
            context.insert_const(name, *value)?;
        }

        for name in self.vars.iter() {
            context.add_var(name)?;
        }

        for (name, stmt) in self.procedures.iter() {
            // TODO(refactor)
            context.insert_proc(name, stmt.clone())?;
        }

        self.stmt.eval(context)
    }
}

impl ASTNodeEval for ASTNode {
    fn eval(&self, context: &mut EvalContext) -> Result<Option<i64>> {
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
mod tests {
    use super::*;
    use crate::{Lexer, Parser};

    #[test]
    fn test_eval_frame() {
        let mut frame = EvalFrame::new();

        frame.add_var("a").unwrap();
        assert!(matches!(
            frame.add_var("a"),
            Err(Pl0Error::RedefinedSymbol(_))
        ));
        assert!(matches!(
            frame.insert_const("a", 1),
            Err(Pl0Error::RedefinedSymbol(_))
        ));
        assert!(matches!(
            frame.get_value("a"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));

        frame.update_var_value("a", 1).unwrap();
        assert!(matches!(frame.get_value("a"), Ok(1)));

        frame.insert_var("b", Some(2)).unwrap();
        assert!(matches!(frame.get_value("b"), Ok(2)));

        frame.insert_var("c", None).unwrap();
        assert!(matches!(
            frame.get_value("c"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));

        frame.insert_const("d", 4).unwrap();
        assert!(matches!(frame.get_value("d"), Ok(4)));

        assert!(matches!(
            frame.insert_proc("a", make_simple_assign_block("a", 1)),
            Err(Pl0Error::RedefinedSymbol(_))
        ));
        assert!(matches!(
            frame.get_proc("P"),
            Err(Pl0Error::UndefinedSymbol(_))
        ));
        frame
            .insert_proc("P", make_simple_assign_block("a", 1))
            .unwrap();
        assert!(frame.check_symbol_exist("P"));
        assert!(matches!(frame.get_proc("P").unwrap(), BlockASTNode { .. }));
    }

    #[test]
    fn test_eval_context_push_pop() {
        let mut context = EvalContext::new();
        assert_eq!(context.depth(), 1);
        context.new_frame();
        assert_eq!(context.depth(), 2);
        context.pop_frame().unwrap();
        assert_eq!(context.depth(), 1);
        context.pop_frame().unwrap();
        assert_eq!(context.depth(), 0);
        assert!(matches!(
            context.pop_frame(),
            Err(Pl0Error::EmptyStackFrame)
        ));
    }

    #[test]
    fn test_eval_context_var() {
        let mut context = EvalContext::new();
        context.add_var("a").unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(
            context.get_value("a"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));

        context.new_frame();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(
            context.get_value("a"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));
        context.update_var_value("a", 1).unwrap();
        assert!(matches!(context.get_value("a").unwrap(), 1));

        context.add_var("a").unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(
            context.get_value("a"),
            Err(Pl0Error::VarUsedBeforeInitialize(_))
        ));
        context.update_var_value("a", 2).unwrap();
        assert!(matches!(context.get_value("a").unwrap(), 2));

        context.pop_frame().unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(context.get_value("a").unwrap(), 1));

        assert!(matches!(
            context.insert_const("a", 100),
            Err(Pl0Error::RedefinedSymbol(_))
        ));
        context.new_frame();
        context.insert_const("a", 100).unwrap();
        assert_eq!(context.get_value("a").unwrap(), 100);
    }

    #[test]
    fn test_eval_context_const() {
        let mut context = EvalContext::new();
        context.insert_const("a", 1).unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(context.get_value("a").unwrap(), 1));

        context.new_frame();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(context.get_value("a").unwrap(), 1));
        context.insert_const("a", 2).unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(context.get_value("a").unwrap(), 2));

        context.pop_frame().unwrap();
        assert!(context.check_symbol_exist("a"));
        assert!(matches!(context.get_value("a").unwrap(), 1));
    }

    #[test]
    fn test_eval_context_proc() {
        let mut context = EvalContext::new();

        context
            .insert_proc("P1", make_simple_assign_block("a", 1))
            .unwrap();
        assert!(context.check_symbol_exist("P1"));
        assert!(matches!(
            context.get_proc("P1").unwrap(),
            BlockASTNode { .. }
        ));

        context.new_frame();
        assert!(context.check_symbol_exist("P1"));
        assert!(matches!(
            context.get_proc("P1").unwrap(),
            BlockASTNode { .. }
        ));
        context
            .insert_proc("P1", make_simple_assign_block("a", 2))
            .unwrap();
        assert!(context.check_symbol_exist("P1"));
        assert!(matches!(
            context.get_proc("P1").unwrap(),
            BlockASTNode { .. }
        ));
        context.pop_frame().unwrap();
        assert!(context.check_symbol_exist("P1"));
        assert!(matches!(
            context.get_proc("P1").unwrap(),
            BlockASTNode { .. }
        ));
    }

    #[test]
    fn test_expr_eval() {
        let mut context = EvalContext::new();

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
        let mut context = EvalContext::new();

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
        let mut context = EvalContext::new();

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
        let mut context = EvalContext::new();

        let expr = make_simple_expr(vec![1]);
        let odd_cond = CondASTNode::OddCond(expr);
        assert_eq!(odd_cond.eval(&mut context).unwrap(), Some(1));

        let expr = make_simple_expr(vec![1, 2, 3]);
        let odd_cond = CondASTNode::OddCond(expr);
        assert_eq!(odd_cond.eval(&mut context).unwrap(), Some(0));
    }

    #[test]
    fn test_std_cond() {
        let mut context = EvalContext::new();

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
        let mut context = EvalContext::new();

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
        let mut context = EvalContext::new();

        context.add_var("a").unwrap();
        let cond = make_simple_std_cond(vec![1], OperatorToken::Greater, vec![0]);
        let stmt_if = StmtASTNode::If(cond, Box::new(make_simple_assign_stmt("a", 1)));
        stmt_if.eval(&mut context).unwrap();
        assert!(matches!(context.get_value("a").unwrap(), 1));
    }

    #[test]
    fn test_stmt_if_false() {
        let mut context = EvalContext::new();

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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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
        let lexer = Lexer::new(src);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut context = EvalContext::new();
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

    fn make_simple_assign_block(name: &str, value: i64) -> BlockASTNode {
        BlockASTNode {
            consts: vec![],
            vars: vec![name.into()],
            procedures: vec![],
            stmt: make_simple_assign_stmt(name, value),
        }
    }
}
