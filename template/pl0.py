#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import string

from enum import IntEnum
from typing import NamedTuple

TEST_PROGRAM = """
var i, s;
begin
    i := 0; s := 0;
    while i < 5 do
    begin
        i := i + 1;
        s := s + i * i
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

        elif self.s[self.i] in '=#+-*/,.;()':
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

class Factor(NamedTuple):
    value: 'str | int | Expression'

class Term(NamedTuple):
    lhs: Factor
    rhs: list[tuple[str, Factor]]

class Expression(NamedTuple):
    mod: str
    lhs: Term
    rhs: list[tuple[str, Term]]

class Const(NamedTuple):
    name  : str
    value : int

class Assign(NamedTuple):
    name : str
    expr : Expression

class Call(NamedTuple):
    name: str

class Begin(NamedTuple):
    body: list['Statement']

class OddCondition(NamedTuple):
    expr: Expression

class StdCondition(NamedTuple):
    op  : str
    lhs : Expression
    rhs : Expression

class Condition(NamedTuple):
    cond: OddCondition | StdCondition

class If(NamedTuple):
    cond: Condition
    then: 'Statement'

class While(NamedTuple):
    cond : Condition
    do   : 'Statement'

class Statement(NamedTuple):
    stmt: Assign | Call | Begin | If | While

class Procedure(NamedTuple):
    name: str
    body: 'Block'

class Block(NamedTuple):
    consts : list[Const]
    vars   : list[str]
    procs  : list[Procedure]
    stmt   : Statement

class Program(NamedTuple):
    block: Block

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
        raise NotImplementedError('proc')
        # TODO: exercise: Implement your own PROCEDURE parsing routine.

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

ps = Parser(Lexer(TEST_PROGRAM))
print(ps.program())