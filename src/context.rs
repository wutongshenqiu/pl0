use crate::{Pl0Error, Result};
use std::collections::HashMap;

#[derive(Debug)]
struct Frame<T> {
    vars: HashMap<String, Option<i64>>,
    consts: HashMap<String, i64>,
    // TODO(optimize): 使用引用而不是拷贝
    // 如何确保 T(例如 BlockASTNode) 的生命周期大于 EvalFrame?
    procs: HashMap<String, T>,
}

impl<T> Frame<T> {
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

    pub fn get_proc(&self, name: &str) -> Result<&T> {
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

    pub fn insert_proc(&mut self, name: &str, proc: T) -> Result<()> {
        if self.check_symbol_exist(name) {
            Err(Pl0Error::RedefinedSymbol(name.into()))
        } else {
            self.procs.insert(name.into(), proc);
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Context<T> {
    st: Vec<Frame<T>>,
}

impl<T> Context<T> {
    pub fn new() -> Self {
        Self {
            st: vec![Frame::new()],
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

    pub fn get_proc(&self, name: &str) -> Result<&T> {
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

    pub fn insert_proc(&mut self, name: &str, proc: T) -> Result<()> {
        self.st
            .last_mut()
            .ok_or(Pl0Error::EmptyStackFrame)?
            .insert_proc(name, proc)
    }

    pub fn new_frame(&mut self) {
        self.st.push(Frame::new());
    }

    pub fn pop_frame(&mut self) -> Result<()> {
        self.st.pop().ok_or(Pl0Error::EmptyStackFrame)?;

        Ok(())
    }

    pub fn depth(&self) -> usize {
        self.st.len()
    }
}

impl<T> Default for Context<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // TODO(refactor): An ugly import
    use crate::ast_eval::tests::make_simple_assign_block;
    use crate::BlockASTNode;

    #[test]
    fn test_frame() {
        let mut frame = Frame::new();

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
    fn test_context_push_pop() {
        let mut context: Context<i64> = Context::new();
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
    fn test_context_var() {
        let mut context: Context<i64> = Context::new();
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
    fn test_context_const() {
        let mut context: Context<i64> = Context::new();
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
    fn test_context_proc() {
        let mut context = Context::new();

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
}
