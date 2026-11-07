# (srfi srfi-9) 模块函数分类索引

SRFI-9 定义 `define-record-type` 宏：用于定义记录类型，
包括类型谓词、构造过程与字段访问器。

本实现直接使用 R7RS `(scheme base)` 提供的 `define-record-type`。

## 常见用法示例

```scheme
(import (srfi srfi-9))

;; 示例1：定义记录类型
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr)
)

;; 示例2：构造与访问
(kar (kons 1 2)) ; => 1
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-9 "函数名"
```

## 函数分类索引

### 一、记录类型定义
- `define-record-type` - 定义记录类型（由 `(scheme base)` 实现）
