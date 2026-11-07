# (srfi srfi-8) 模块函数分类索引

SRFI-8 提供 `receive` 宏：把多值表达式的返回结果绑定到形参列表并执行表达式体。
它为 `call-with-values` 提供了更简洁、可读性更高的接收多返回值语法抽象。

## 常见用法示例

```scheme
(import (srfi srfi-8))

;; 示例1：绑定固定长度的多值
(receive (a b) (values 1 2) (+ a b)) ; => 3

;; 示例2：绑定可变长度的多值
(receive (a . rest) (values 1 2 3) (cons a rest)) ; => (1 2 3)

;; 示例3：单个变量捕获全部多值
(receive all (values 10 20) all) ; => (10 20)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-8 "函数名"
```

## 函数分类索引

### 一、多值接收宏
- `receive` - 将多值表达式的返回结果绑定到形参列表并执行表达式体
