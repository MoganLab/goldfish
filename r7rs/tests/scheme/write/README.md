# (scheme write) 模块函数分类索引

`(scheme write)` 提供了将 Scheme 对象输出到端口的基本 I/O 函数。
包括可读性输出 (write)、用户友好展示 (display) 以及共享结构处理
(write-shared, write-simple) 等功能。

注：`write-string` 属于 `(scheme base)`，参见 `gf doc scheme/base`。

## 常见用法示例

```scheme
(import (scheme write))

;; 示例1：使用 write 输出可读形式
(define port (open-output-string))
(write '(1 2 3) port)
(get-output-string port)       ; => "(1 2 3)"

;; 示例2：使用 display 输出用户友好的形式
(define port2 (open-output-string))
(display "hello" port2)
(get-output-string port2)      ; => "hello"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/write "函数名"
```

## 函数分类索引

### 一、基本输出

- `write` - 按可读回的 Scheme 表示写入对象
- `display` - 按用户友好的方式写入对象

### 二、共享结构处理

- `write-shared` - 处理循环结构的写入（当前与 write 兼容）
- `write-simple` - 简单写入（当前与 write 兼容）
