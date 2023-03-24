# Sonic

## Structure

```bash
.
├── examples                # pl0 代码示例
│   ├── basic.pl0
│   ├── complicate.pl0
│   ├── fact.pl0
│   ├── fib.pl0
│   └── primes.pl0
├── src                     # rust 源代码目录
│   ├── ast_eval.rs         # 实现 ast 的直接执行
│   ├── ast_ir.rs           # 实现 ast 生成 ir
│   ├── bin            
│   │   └── pl0.rs          # pl0 命令行程序
│   ├── context.rs          # 支持 ast eval 和 ir eval 的上下文管理器
│   ├── error.rs            # 错误定义
│   ├── interpreter.rs      # 解释器(ir eval)
│   ├── lexer.rs            # 词法分析器
│   ├── lib.rs            
│   └── parser.rs           # 语法分析器
├── template                # 参考代码目录
│   └── pl0.py              # pl0 参考代码
└── tests                   # 单元测试目录
    └── pl0.rs              # 针对 cli 的单元测试
```

## Task

- [x] 完成 procedure 语法分析
- [x] 实现 procedure 和 call 的 ast-eval, ir-gen 以及对应的 ir-eval