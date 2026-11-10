# (scheme lazy) 模块函数分类索引

R7RS `scheme lazy` 提供 Scheme 的延迟求值（lazy evaluation）原语，
实现基于 SRFI 45 语义，支持记忆化、重入 force 与迭代惰性算法的有界空间运行。

## 常见用法示例

```scheme
(import (scheme lazy) (scheme base))

;; 示例1：延迟求值与记忆化（表达式只求值一次）
(define count 0)
(define p (delay (begin (set! count (+ count 1)) count)))
(force p)   ; => 1
(force p)   ; => 1，count 仍为 1

;; 示例2：make-promise 急切构造
(force (make-promise 42))          ; => 42
(force (make-promise (delay 3)))   ; => 3，promise 原样返回

;; 示例3：迭代惰性算法（有界空间，R7RS 4.2.5 官方 stream-filter 模式）
(define (stream-filter p? s)
  (delay-force
    (let ((cell (force s)))
      (cond ((null? cell) '())
            ((p? (car cell)) (cons (car cell) (stream-filter p? (cdr cell))))
            (else (stream-filter p? (cdr cell)))))))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/lazy "函数名"
```

## 函数分类索引

### 一、promise 构造（R7RS 4.2.5 Delayed evaluation）
- `delay` - 延迟求值语法，封装表达式为 promise
- `delay-force` - 递归延迟求值，保证迭代惰性算法有界空间运行
- `make-promise` - 过程版急切构造，已是 promise 时原样返回

### 二、promise 求值与判断
- `force` - 强制求值 promise 并记忆化结果；非 promise 参数原样返回
- `promise?` - 判断对象是否为 promise
