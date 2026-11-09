# (srfi srfi-16) 模块函数分类索引

SRFI-16 提供 `case-lambda` 语法：用于定义一个按参数个数分派的过程。

本实现直接使用 R7RS `(scheme case-lambda)` 提供的 `case-lambda`。

## 常见用法示例

```scheme
(import (srfi srfi-16))

;; 示例1：按参数个数分派
(define f
  (case-lambda
   (() "zero args")
   ((x) (* x 2))
   ((x y) (+ x y))
  ))

(f 2) ; => 4
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-16 "函数名"
```

## 函数分类索引

### 一、过程构造
- `case-lambda` - 定义按参数个数分派的过程（由 `(scheme case-lambda)` 实现）
