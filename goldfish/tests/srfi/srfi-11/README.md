# (srfi srfi-11) 模块函数分类索引

`(srfi srfi-11)` 是 SRFI-11 "binding let" 的兼容入口，重导出 `let-values` 与
`let*-values` 两个多值绑定宏。

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc srfi/srfi-11 "函数名"
```

## 函数分类索引

### 一、多值绑定语法

- `let-values` - 并行绑定多值表达式的返回结果到变量
- `let*-values` - 顺序依赖绑定多值表达式的返回结果

## 说明

R7RS-small 已将这两个宏收进 `(scheme base)`，实现也随之内联到 `(scheme base)`，
本库仅重导出以保留原有 import 路径可用；按函数拆分的单元测试统一放在其实现库
`tests/scheme/base/` 目录。
