use crate::{
    ast_ir::{IrArg, IrValue},
    Context, Ir, IrOpCode, Pl0Error, Result,
};
use std::io;

pub struct Intepreter {}

type ContextIrBuf = Context<Vec<Ir>>;

#[inline(always)]
fn pop_st(st: &mut Vec<i64>) -> Result<i64> {
    st.pop().ok_or(Pl0Error::EmptyIrStack)
}

impl Intepreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn ir_eval(buf: &Vec<Ir>, context: &mut ContextIrBuf) -> Result<()> {
        // 一开始的想法是将 pc 寄存器和运行栈 st 作为 `Intepreter` 的一个 field
        // 但在调用子程序时需要对 ir_eval 进行递归调用, 还要对 pc/st 进行维护, 增加了复杂性
        let mut pc: usize = 0;
        let mut st: Vec<i64> = vec![];

        while let Some(ir) = buf.get(pc) {
            pc += 1;

            match ir.op {
                IrOpCode::Add => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push(v2 + v1);
                }
                IrOpCode::Sub => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push(v1 - v2);
                }
                IrOpCode::Neg => {
                    let v = pop_st(&mut st)?;
                    st.push(-v);
                }
                IrOpCode::Mul => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push(v2 * v1);
                }
                IrOpCode::Div => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    if v2 == 0 {
                        return Err(Pl0Error::DivisionByZero);
                    }
                    st.push(v1 / v2);
                }
                IrOpCode::Eq => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 == v2) as i64);
                }
                IrOpCode::Ne => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 != v2) as i64);
                }
                IrOpCode::Lt => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 < v2) as i64);
                }
                IrOpCode::Lte => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 <= v2) as i64);
                }
                IrOpCode::Gt => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 > v2) as i64);
                }
                IrOpCode::Gte => {
                    let v2 = pop_st(&mut st)?;
                    let v1 = pop_st(&mut st)?;
                    st.push((v1 >= v2) as i64);
                }
                IrOpCode::Odd => {
                    let v = pop_st(&mut st)?;
                    st.push(v & 1);
                }
                IrOpCode::LoadVar => {
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        st.push(context.get_value(name)?);
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::LoadLit => {
                    if let Some(IrArg::Number(number)) = &ir.arg {
                        st.push(*number);
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::Store => {
                    let v = pop_st(&mut st)?;
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        context.update_var_value(name, v)?;
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::Jump => {
                    if let Some(IrArg::Number(i)) = &ir.arg {
                        let i = *i;
                        if i < 0 || i >= buf.len() as i64 {
                            return Err(Pl0Error::PCOutOfBoundary);
                        } else {
                            pc = i as usize;
                        }
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::BrFalse => {
                    let v = pop_st(&mut st)?;
                    if v & 1 == 0 {
                        if let Some(IrArg::Number(i)) = &ir.arg {
                            let i = *i;
                            if i < 0 || i >= buf.len() as i64 {
                                return Err(Pl0Error::PCOutOfBoundary);
                            } else {
                                pc = i as usize;
                            }
                        } else {
                            return Err(Pl0Error::InvalidIrArg);
                        }
                    }
                }
                IrOpCode::DefVar => {
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        context.add_var(name)?;
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::DefLit => {
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        if let Some(IrValue::Number(number)) = &ir.value {
                            context.insert_const(name, *number)?;
                        } else {
                            return Err(Pl0Error::InvalidIrValue);
                        }
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::DefProc => {
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        if let Some(IrValue::Irs(irs)) = &ir.value {
                            // TODO(optimize): 使用引用代替拷贝
                            context.insert_proc(name, irs.to_vec())?;
                        } else {
                            return Err(Pl0Error::InvalidIrValue);
                        }
                    } else {
                        return Err(Pl0Error::InvalidIrArg);
                    }
                }
                IrOpCode::Call => {
                    if let Some(IrArg::Ident(name)) = &ir.arg {
                        let ir_buf = context.get_proc(name)?.to_vec();
                        context.new_frame();
                        // TODO(optimize): 使用引用代替拷贝
                        Self::ir_eval(&ir_buf, context)?;
                        context.pop_frame()?;
                    }
                }
                IrOpCode::Input => {
                    let mut line = String::new();
                    io::stdin().read_line(&mut line)?;
                    let value: i64 = line.trim().parse()?;
                    st.push(value);
                }
                IrOpCode::Output => {
                    let v = pop_st(&mut st)?;
                    println!("{}", v);
                }
                IrOpCode::Halt => break,
            }
        }

        Ok(())
    }
}

impl Default for Intepreter {
    fn default() -> Self {
        Self::new()
    }
}
