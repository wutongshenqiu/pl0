#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import string

from enum import IntEnum
from typing import NamedTuple

TEST_PROGRAM = """
var a, b, n, t;
begin
    ?n;
    a := 0;
    b := 1;
    while n > 0 do
    begin
        !b;
        n := n - 1;
        t := a + b;
        a := b;
        b := t
    end
end.
"""

IDENT_FIRST  = string.ascii_letters + '_'
IDENT_REMAIN = string.ascii_letters + string.digits + '_'

KEYWORD_SET = {
    'const',
    'var',
    'procedure',
    'call',
    'begin',
    'end',
    'if',
    'then',
    'while',
    'do',
    'odd',
}

class TokenKind(IntEnum):
    Op      = 0
    Num     = 1
    Name    = 2
    Keyword = 3
    Eof     = 4

class Token(NamedTuple):
    ty  : TokenKind
    val : int | str

    @classmethod
    def op(cls, op: str) -> 'Token':
        return cls(ty = TokenKind.Op, val = op)

    @classmethod
    def num(cls, num: int) -> 'Token':
        return cls(ty = TokenKind.Num, val = num)

    @classmethod
    def name(cls, name: str) -> 'Token':
        return cls(ty = TokenKind.Name, val = name)

    @classmethod
    def keyword(cls, keyword: str) -> 'Token':
        return cls(ty = TokenKind.Keyword, val = keyword)

    @classmethod
    def eof(cls) -> 'Token':
        return cls(ty = TokenKind.Eof, val = 0)

class Lexer:
    i: int
    s: str

    def __init__(self, src: str):
        self.i = 0
        self.s = src

    @property
    def eof(self) -> bool:
        return self.i >= len(self.s)

    def _skip_blank(self):
        while not self.eof and self.s[self.i].isspace():
            self.i += 1

    def next(self) -> Token:
        val = ''
        self._skip_blank()

        if self.eof:
            return Token.eof()

        elif self.s[self.i].isdigit():
            while self.s[self.i].isdigit():
                val += self.s[self.i]
                self.i += 1
            return Token.num(int(val))

        elif self.s[self.i] in IDENT_FIRST:
            while self.s[self.i] in IDENT_REMAIN:
                val += self.s[self.i]
                self.i += 1

            if val in KEYWORD_SET:
                return Token.keyword(val)
            else:
                return Token.name(val)

        elif self.s[self.i] in '=#+-*/,.;()?!':
            ch = self.s[self.i]
            self.i += 1
            return Token.op(ch)

        elif self.s[self.i] == ':':
            self.i += 1

            if self.eof or self.s[self.i] != '=':
                raise SyntaxError('"=" expected')

            self.i += 1
            return Token.op(':=')

        elif self.s[self.i] in '><':
            ch = self.s[self.i]
            self.i += 1

            if not self.eof and self.s[self.i] == '=':
                self.i += 1
                return Token.op(ch + '=')
            else:
                return Token.op(ch)

        else:
            raise SyntaxError('invalid character ' + repr(self.s[self.i]))

class IrOpCode(IntEnum):
    Add     = 0
    Sub     = 1
    Mul     = 2
    Div     = 3
    Neg     = 4
    Eq      = 5
    Ne      = 6
    Lt      = 7
    Lte     = 8
    Gt      = 9
    Gte     = 10
    Odd     = 11
    LoadVar = 12
    LoadLit = 13
    Store   = 14
    Jump    = 15
    BrFalse = 16
    DefVar  = 17
    DefLit  = 18
    DefProc = 19
    Input   = 100
    Output  = 101
    Halt    = 255

class Ir(NamedTuple):
    op    : IrOpCode
    args  : str | int | None        = None
    value : int | list['Ir'] | None = None

class EvalContext(NamedTuple):
    vars   : dict[str, int | None]
    procs  : dict[str, 'Procedure | list[Ir]']
    consts : dict[str, int]

class Factor(NamedTuple):
    value: 'str | int | Expression'

    def gen(self, buf: list[Ir]):
        if isinstance(self.value, int):
            buf.append(Ir(IrOpCode.LoadLit, self.value))
        elif isinstance(self.value, str):
            buf.append(Ir(IrOpCode.LoadVar, self.value))
        elif isinstance(self.value, Expression):
            self.value.gen(buf)
        else:
            raise RuntimeError('invalid factor value')

    def eval(self, ctx: EvalContext) -> int | None:
        if isinstance(self.value, int):
            return self.value

        elif isinstance(self.value, str):
            if self.value in ctx.vars:
                key = self.value
                ret = ctx.vars[key]

                if ret is None:
                    raise RuntimeError('variable %s referenced before initialize' % key)
                else:
                    return ret

            elif self.value in ctx.consts:
                return ctx.consts[self.value]

            else:
                raise RuntimeError('undefined symbol: ' + self.value)

        elif isinstance(self.value, Expression):
            val = self.value.eval(ctx)
            assert val is not None, 'invalid nested expression'
            return val

        else:
            raise RuntimeError('invalid factor value')

class Term(NamedTuple):
    lhs: Factor
    rhs: list[tuple[str, Factor]]

    def gen(self, buf: list[Ir]):
        self.lhs.gen(buf)

        for op, rhs in self.rhs:
            rhs.gen(buf)

            if op == '*':
                buf.append(Ir(IrOpCode.Mul))
            elif op == '/':
                buf.append(Ir(IrOpCode.Div))
            else:
                raise RuntimeError('invalid expression operator')

    def eval(self, ctx: EvalContext) -> int | None:
        ret = self.lhs.eval(ctx)
        assert ret is not None, 'invalid term lhs'

        for op, rhs in self.rhs:
            val = rhs.eval(ctx)
            assert val is not None, 'invalid expression rhs'

            if op == '*':
                ret *= val
            elif op == '/':
                if val == 0:
                    raise RuntimeError('division by zero')
                else:
                    ret /= val
            else:
                raise RuntimeError('invalid expression operator')

        return ret

class Expression(NamedTuple):
    mod: str
    lhs: Term
    rhs: list[tuple[str, Term]]

    def gen(self, buf: list[Ir]):
        self.lhs.gen(buf)

        if self.mod == '-':
            buf.append(Ir(IrOpCode.Neg))
        elif self.mod not in {'', '+'}:
            raise RuntimeError('invalid expression sign')

        for op, rhs in self.rhs:
            rhs.gen(buf)

            if op == '+':
                buf.append(Ir(IrOpCode.Add))
            elif op == '-':
                buf.append(Ir(IrOpCode.Sub))
            else:
                raise RuntimeError('invalid expression operator')

    def eval(self, ctx: EvalContext) -> int | None:
        if self.mod == '-':
            sign = -1
        elif self.mod in {'', '+'}:
            sign = 1
        else:
            raise RuntimeError('invalid expression sign')

        ret = self.lhs.eval(ctx) * sign
        assert ret is not None, 'invalid expression lhs'

        for op, rhs in self.rhs:
            val = rhs.eval(ctx)
            assert val is not None, 'invalid expression rhs'

            if op == '+':
                ret += val
            elif op == '-':
                ret -= val
            else:
                raise RuntimeError('invalid expression operator')

        return ret

class Const(NamedTuple):
    name  : str
    value : int

class Assign(NamedTuple):
    name : str
    expr : Expression

    def gen(self, buf: list[Ir]):
        self.expr.gen(buf)
        buf.append(Ir(IrOpCode.Store, self.name))

    def eval(self, ctx: EvalContext) -> int | None:
        if self.name not in ctx.vars:
            raise RuntimeError('undefined variable: ' + self.name)

        val = self.expr.eval(ctx)
        assert val is not None, 'invalid assignment expression'

        ctx.vars[self.name] = val
        return None

class Call(NamedTuple):
    name: str

    def gen(self, buf: list[Ir]):
        # TODO: implement call generation
        raise NotImplementedError('implement call generation')

    def eval(self, ctx: EvalContext) -> int | None:
        # TODO: implement call evaluation
        raise NotImplementedError('implement call evaluation')

class Begin(NamedTuple):
    body: list['Statement']

    def gen(self, buf: list[Ir]):
        for stmt in self.body:
            stmt.gen(buf)

    def eval(self, ctx: EvalContext) -> int | None:
        for stmt in self.body:
            stmt.eval(ctx)

class OddCondition(NamedTuple):
    expr: Expression

    def gen(self, buf: list[Ir]):
        self.expr.gen(buf)
        buf.append(Ir(IrOpCode.Odd))

    def eval(self, ctx: EvalContext) -> int | None:
        val = self.expr.eval(ctx)
        assert val is not None, 'invalid odd expression'
        return val & 1

class StdCondition(NamedTuple):
    op  : str
    lhs : Expression
    rhs : Expression

    def gen(self, buf: list[Ir]):
        self.lhs.gen(buf)
        self.rhs.gen(buf)

        if self.op == '>':
            buf.append(Ir(IrOpCode.Gt))
        elif self.op == '<':
            buf.append(Ir(IrOpCode.Lt))
        elif self.op == '>=':
            buf.append(Ir(IrOpCode.Gte))
        elif self.op == '<=':
            buf.append(Ir(IrOpCode.Lte))
        elif self.op == '=':
            buf.append(Ir(IrOpCode.Eq))
        elif self.op == '#':
            buf.append(Ir(IrOpCode.Ne))
        else:
            raise RuntimeError('invalid std condition operator: ' + self.op)

    def eval(self, ctx: EvalContext) -> int | None:
        lhs = self.lhs.eval(ctx)
        rhs = self.rhs.eval(ctx)

        assert lhs is not None, 'invalid std condition lhs expression'
        assert rhs is not None, 'invalid std condition rhs expression'

        if self.op == '>':
            return 1 if lhs > rhs else 0
        elif self.op == '<':
            return 1 if lhs < rhs else 0
        elif self.op == '>=':
            return 1 if lhs >= rhs else 0
        elif self.op == '<=':
            return 1 if lhs <= rhs else 0
        elif self.op == '=':
            return 1 if lhs == rhs else 0
        elif self.op == '#':
            return 1 if lhs != rhs else 0
        else:
            raise RuntimeError('invalid std condition operator: ' + self.op)

class Condition(NamedTuple):
    cond: OddCondition | StdCondition

    def gen(self, buf: list[Ir]):
        self.cond.gen(buf)

    def eval(self, ctx: EvalContext) -> int | None:
        return self.cond.eval(ctx)

class If(NamedTuple):
    cond: Condition
    then: 'Statement'

    def gen(self, buf: list[Ir]):
        self.cond.gen(buf)
        i = len(buf)
        buf.append(Ir(IrOpCode.BrFalse))
        self.then.gen(buf)
        buf[i] = Ir(IrOpCode.BrFalse, len(buf))

    def eval(self, ctx: EvalContext) -> int | None:
        val = self.cond.eval(ctx)
        assert val is not None, 'invalid if condition expression'

        if val != 0:
            self.then.eval(ctx)

class While(NamedTuple):
    cond : Condition
    do   : 'Statement'

    def gen(self, buf: list[Ir]):
        i = len(buf)
        self.cond.gen(buf)
        j = len(buf)
        buf.append(Ir(IrOpCode.BrFalse))
        self.do.gen(buf)
        buf.append(Ir(IrOpCode.Jump, i))
        buf[j] = Ir(IrOpCode.BrFalse, len(buf))

    def eval(self, ctx: EvalContext) -> int | None:
        while True:
            val = self.cond.eval(ctx)
            assert val is not None, 'invalid while condition expression'

            if val == 0:
                break
            else:
                self.do.eval(ctx)

class InputOutput(NamedTuple):
    name     : str
    is_input : bool

    def gen(self, buf: list[Ir]):
        if self.is_input:
            buf.append(Ir(IrOpCode.Input))
            buf.append(Ir(IrOpCode.Store, self.name))
        else:
            buf.append(Ir(IrOpCode.LoadVar, self.name))
            buf.append(Ir(IrOpCode.Output))

    def eval(self, ctx: EvalContext) -> int | None:
        if not self.is_input:
            print(Factor(self.name).eval(ctx))
        elif self.name in ctx.vars:
            ctx.vars[self.name] = int(input())
        else:
            raise RuntimeError('undefined variable: ' + self.name)

class Statement(NamedTuple):
    stmt: Assign | Call | Begin | If | While | InputOutput

    def gen(self, buf: list[Ir]):
        self.stmt.gen(buf)

    def eval(self, ctx: EvalContext) -> int | None:
        return self.stmt.eval(ctx)

class Procedure(NamedTuple):
    name: str
    body: 'Block'

    def gen(self, buf: list[Ir]):
        self.body.gen(buf)

    def eval(self, ctx: EvalContext) -> int | None:
        # TODO: implement proc evaluation
        raise NotImplementedError('implement proc evaluation')

class Block(NamedTuple):
    consts : list[Const]
    vars   : list[str]
    procs  : list[Procedure]
    stmt   : Statement

    def gen(self, buf: list[Ir]):
        for cc in self.consts:
            buf.append(Ir(IrOpCode.DefLit, cc.name, cc.value))

        for vv in self.vars:
            buf.append(Ir(IrOpCode.DefVar, vv))

        for pp in self.procs:
            proc = []
            pp.gen(proc)
            buf.append(Ir(IrOpCode.DefProc, pp.name, proc))

        self.stmt.gen(buf)

    def eval(self, ctx: EvalContext) -> int | None:
        for cc in self.consts:
            if cc.name in ctx.consts:
                raise RuntimeError('constant redefinition: ' + cc.name)
            else:
                ctx.consts[cc.name] = cc.value

        for vv in self.vars:
            if vv in ctx.vars or vv in ctx.consts:
                raise RuntimeError('variable redefinition: ' + vv)
            else:
                ctx.vars[vv] = None

        for pp in self.procs:
            ctx.procs[pp.name] = pp

        self.stmt.eval(ctx)
        return None

class Program(NamedTuple):
    block: Block

    def gen(self, buf: list[Ir]):
        self.block.gen(buf)
        buf.append(Ir(IrOpCode.Halt))

    def eval(self, ctx: EvalContext) -> int | None:
        return self.block.eval(ctx)

class Parser:
    lx: Lexer

    def __init__(self, lx: Lexer):
        self.lx = lx

    def check(self, ty: TokenKind, val: str | int) -> bool:
        p = self.lx.i
        tk = self.lx.next()

        if tk.ty == ty and tk.val == val:
            return True

        self.lx.i = p
        return False

    def expect(self, ty: TokenKind, val: str | int | None = None):
        tk = self.lx.next()
        tty, tval = tk.ty, tk.val

        if tty != ty:
            raise SyntaxError('%s expected, got %s' % (ty, tty))

        if val is not None:
            if tval != val:
                raise SyntaxError('"%s" expected, got "%s"' % (val, tval))

    def program(self) -> Program:
        block = self.block()
        self.expect(TokenKind.Op, '.')
        return Program(block)

    def block(self) -> Block:
        var = []
        proc = []
        const = []

        if self.check(TokenKind.Keyword, 'const'):
            const = self.const()

        if self.check(TokenKind.Keyword, 'var'):
            var = self.var()

        while self.check(TokenKind.Keyword, 'procedure'):
            proc.append(self.procedure())

        stmt = self.statement()
        return Block(const, var, proc, stmt)

    def const(self) -> list[Const]:
        ret = []
        while True:
            name = self.lx.next()
            ty = name.ty

            if ty != TokenKind.Name:
                raise SyntaxError('name expected')

            self.expect(TokenKind.Op, '=')
            num = self.lx.next()

            if num.ty != TokenKind.Num:
                raise SyntaxError('number expected')
            else:
                ret.append(Const(name.val, num.val))

            if self.check(TokenKind.Op, ';'):
                return ret
            else:
                self.expect(TokenKind.Op, ',')

    def var(self) -> list[str]:
        ret = []
        while True:
            name = self.lx.next()
            ty = name.ty

            if ty != TokenKind.Name:
                raise SyntaxError('name expected')
            else:
                ret.append(name.val)

            if self.check(TokenKind.Op, ';'):
                return ret
            else:
                self.expect(TokenKind.Op, ',')

    def procedure(self) -> Procedure:
        ident = self.lx.next()
        ty = ident.ty

        if ty != TokenKind.Name:
            raise SyntaxError('name expected')

        self.expect(TokenKind.Op, ';')
        block = self.block()
        self.expect(TokenKind.Op, ';')
        return Procedure(ident.val, block)

    def statement(self) -> Statement:
        if self.check(TokenKind.Keyword, 'call'):
            ident = self.lx.next()
            if ident.ty != TokenKind.Name:
                raise SyntaxError('name expected')
            else:
                return Statement(Call(ident))

        elif self.check(TokenKind.Keyword, 'begin'):
            body = []

            while True:
                stmt = self.statement()
                body.append(stmt)

                if self.check(TokenKind.Keyword, 'end'):
                    break
                else:
                    self.expect(TokenKind.Op, ';')

            return Statement(Begin(body))

        elif self.check(TokenKind.Keyword, 'if'):
            cond = self.condition()
            self.expect(TokenKind.Keyword, 'then')
            return Statement(If(cond, self.statement()))

        elif self.check(TokenKind.Keyword, 'while'):
            cond = self.condition()
            self.expect(TokenKind.Keyword, 'do')
            return Statement(While(cond, self.statement()))

        elif self.check(TokenKind.Op, '?'):
            name = self.lx.next()
            ty = name.ty

            if ty != TokenKind.Name:
                raise SyntaxError('name expected')
            else:
                return Statement(InputOutput(name.val, is_input = True))

        elif self.check(TokenKind.Op, '!'):
            name = self.lx.next()
            ty = name.ty

            if ty != TokenKind.Name:
                raise SyntaxError('name expected')
            else:
                return Statement(InputOutput(name.val, is_input = False))

        else:
            tk = self.lx.next()
            ty = tk.ty

            if ty != TokenKind.Name:
                raise SyntaxError('name expected')

            self.expect(TokenKind.Op, ':=')
            return Statement(Assign(tk.val, self.expression()))

    def condition(self) -> Condition:
        if self.check(TokenKind.Keyword, 'odd'):
            return Condition(self.odd_condition())
        else:
            return Condition(self.std_condition())

    def odd_condition(self) -> OddCondition:
        return OddCondition(self.expression())

    def std_condition(self) -> StdCondition:
        lhs = self.expression()
        cmp = self.lx.next()

        if cmp.ty != TokenKind.Op:
            raise SyntaxError('operator expected')

        if cmp.val not in {'=', '#', '<', '<=', '>', '>='}:
            raise SyntaxError('condition operator expected')

        rhs = self.expression()
        return StdCondition(cmp.val, lhs, rhs)

    def expression(self) -> Expression:
        if self.check(TokenKind.Op, '+'):
            mod = '+'
        elif self.check(TokenKind.Op, '-'):
            mod = '-'
        else:
            mod = ''

        rhs = []
        lhs = self.term()

        while True:
            if self.check(TokenKind.Op, '+'):
                rhs.append(('+', self.term()))
            elif self.check(TokenKind.Op, '-'):
                rhs.append(('-', self.term()))
            else:
                return Expression(mod, lhs, rhs)

    def term(self) -> Term:
        rhs = []
        lhs = self.factor()

        while True:
            if self.check(TokenKind.Op, '*'):
                rhs.append(('*', self.factor()))
            elif self.check(TokenKind.Op, '/'):
                rhs.append(('/', self.factor()))
            else:
                return Term(lhs, rhs)

    def factor(self) -> Factor:
        tk = self.lx.next()
        ty, val = tk.ty, tk.val

        if ty in {TokenKind.Num, TokenKind.Name}:
            return Factor(val)

        if ty != TokenKind.Op or val != '(':
            raise SyntaxError('"(" expected')

        expr = self.expression()
        self.expect(TokenKind.Op, ')')
        return Factor(expr)

def ir_eval(buf: list[Ir], ctx: EvalContext):
    pc = 0
    sp = []

    while pc < len(buf):
        ir = buf[pc]
        pc += 1

        if ir.op == IrOpCode.Add:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(v1 + v2)

        elif ir.op == IrOpCode.Sub:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(v1 - v2)

        elif ir.op == IrOpCode.Mul:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(v1 * v2)

        elif ir.op == IrOpCode.Div:
            v2 = sp.pop()
            v1 = sp.pop()

            if v2 == 0:
                raise RuntimeError('division by zero')
            else:
                sp.append(v1 / v2)

        elif ir.op == IrOpCode.Neg:
            sp[-1] = -sp[-1]

        elif ir.op == IrOpCode.Eq:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 == v2))

        elif ir.op == IrOpCode.Ne:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 != v2))

        elif ir.op == IrOpCode.Lt:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 < v2))

        elif ir.op == IrOpCode.Lte:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 <= v2))

        elif ir.op == IrOpCode.Gt:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 > v2))

        elif ir.op == IrOpCode.Gte:
            v2 = sp.pop()
            v1 = sp.pop()
            sp.append(int(v1 >= v2))

        elif ir.op == IrOpCode.Odd:
            sp[-1] = sp[-1] & 1

        elif ir.op == IrOpCode.LoadVar:
            if not isinstance(ir.args, str):
                raise RuntimeError('invalid loadvar args')

            elif ir.args in ctx.vars:
                key = ir.args
                val = ctx.vars[key]

                if val is None:
                    raise RuntimeError('variable %s referenced before initialization' % key)
                else:
                    sp.append(val)

            elif ir.args in ctx.consts:
                sp.append(ctx.consts[ir.args])

            else:
                raise RuntimeError('undefined variable: ' + ir.args)

        elif ir.op == IrOpCode.LoadLit:
            if not isinstance(ir.args, int):
                raise RuntimeError('invalid loadvar args')
            else:
                sp.append(ir.args)

        elif ir.op == IrOpCode.Store:
            if not isinstance(ir.args, str):
                raise RuntimeError('invalid store args')
            elif ir.args not in ctx.vars:
                raise RuntimeError('undefined variable: ' + ir.args)
            else:
                ctx.vars[ir.args] = sp.pop()

        elif ir.op == IrOpCode.Jump:
            if not isinstance(ir.args, int):
                raise RuntimeError('invalid jump args')
            elif 0 <= ir.args < len(buf):
                pc = ir.args
            else:
                raise RuntimeError('branch out of bounds')

        elif ir.op == IrOpCode.BrFalse:
            if not sp.pop():
                if not isinstance(ir.args, int):
                    raise RuntimeError('invalid brfalse args')
                elif 0 <= ir.args < len(buf):
                    pc = ir.args
                else:
                    raise RuntimeError('branch out of bounds')

        elif ir.op == IrOpCode.DefVar:
            if not isinstance(ir.args, str):
                raise RuntimeError('invalid defvar args')
            elif ir.args in ctx.vars or ir.args in ctx.procs or ir.args in ctx.consts:
                raise RuntimeError('variable redeclared: ' + ir.args)
            else:
                ctx.vars[ir.args] = None

        elif ir.op == IrOpCode.DefLit:
            if not isinstance(ir.args, str) or not isinstance(ir.value, int):
                raise RuntimeError('invalid deflit args')
            elif ir.args in ctx.vars or ir.args in ctx.procs or ir.args in ctx.consts:
                raise RuntimeError('variable redeclared: ' + ir.args)
            else:
                ctx.consts[ir.args] = ir.value

        elif ir.op == IrOpCode.DefProc:
            if not isinstance(ir.args, str) or not isinstance(ir.value, list[Ir]):
                raise RuntimeError('invalid defproc args')
            elif ir.args in ctx.vars or ir.args in ctx.procs or ir.args in ctx.consts:
                raise RuntimeError('variable redeclared: ' + ir.args)
            else:
                ctx.procs[ir.args] = ir.value

        elif ir.op == IrOpCode.Input:
            sp.append(int(input()))

        elif ir.op == IrOpCode.Output:
            print(sp.pop())

        elif ir.op == IrOpCode.Halt:
            break

        else:
            raise RuntimeError('invalid instruction')

def main():
    ps = Parser(Lexer(TEST_PROGRAM))
    buf = []
    ast = ps.program()
    ast.gen(buf)
    ir_eval(buf, EvalContext({}, {}, {}))

if __name__ == '__main__':
    main()