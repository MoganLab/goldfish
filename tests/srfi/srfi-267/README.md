# (srfi srfi-267) 模块函数分类索引

SRFI-267 定义 raw string 字面量：`#"<delimiter>"<content>"<delimiter>"`，
用自定义 delimiter 包围原始字符串，从而在不转义引号与反斜杠的前提下书写多行文本。

同时提供配套的读写过程：读取 raw string、判断错误对象、生成可用 delimiter、
按 raw string 形式写出字符串。

## 常见用法示例

```scheme
(import (srfi srfi-267))

;; 示例1：读取 raw string
(read-raw-string (open-input-string "#\"\"hello\"\"")) ; => "hello"

;; 示例2：生成可用 delimiter
(define delimiter (generate-delimiter "含 \" 引号的文本"))
(can-delimit? "含 \" 引号的文本" delimiter) ; => #t

;; 示例3：写出 raw string
(write-raw-string "hello" "" (current-output-port))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-267 "函数名"
```

## 函数分类索引

### 一、错误对象判定
- `raw-string-read-error?` - 判断对象是否为 raw string 读取错误
- `raw-string-write-error?` - 判断对象是否为 raw string 写出错误

### 二、读取
- `read-raw-string` - 从端口读取 raw string 字面量
- `read-raw-string-after-prefix` - 在已读过前缀后继续读取 raw string 内容

### 三、delimiter
- `can-delimit?` - 判断某个 delimiter 是否可用于表示给定字符串
- `generate-delimiter` - 为字符串生成可用的 delimiter

### 四、写出
- `write-raw-string` - 把字符串按 raw string 字面量的形式写出
