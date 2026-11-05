# (srfi srfi-11) 模块函数分类索引

`(srfi srfi-11)` 提供 `let-values` 与 `let*-values` 语法，
用于将多值表达式的返回结果绑定到变量，
是处理 `values` 多值返回的基础设施。

## 常见用法示例

```scheme
(import (srfi srfi-11))

;; 示例1：并行绑定多值结果
(let-values (((q r) (floor/ 7 2)))
  (list q r))                    ; => (3 1)

;; 示例2：顺序依赖绑定
(let*-values (((a) (values 1))
              ((b) (values (+ a 1))))
  b)                             ; => 2
```

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
