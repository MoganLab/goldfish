# (liii check) 模块函数分类索引

`(liii check)` 是 Goldfish Scheme 的轻量级单元测试框架，基于 SRFI-78 提供基础断言、异常断言、结果汇总与浮点近似断言。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/check "函数名"
```

## 函数分类索引

### 一、基础断言
- `test` - `check` 的简写别名，用于快速精确断言
- `check-true` - 断言表达式求值结果为 `#t`
- `check-false` - 断言表达式求值结果为 `#f`
- `check-approx` - 断言两个数值在给定容差内近似相等

### 二、异常捕获与汇总
- `check-catch` - 断言表达式求值时抛出指定的错误标识
- `check-report` - 输出测试汇总信息，并在存在失败断言时退出进程
