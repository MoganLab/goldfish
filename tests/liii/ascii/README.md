# (liii ascii) 模块函数分类索引

`ascii` 提供 ASCII 字符集的分类、转换与大小写无关比较能力，
所有函数都接受字符、码点或字符串等多种输入形式。
`(liii ascii)` 基于 `(srfi srfi-175)`，并额外提供 ASCII 括号判定函数。

## 常见用法示例

```scheme
(import (liii ascii))

;; 示例1：判断一个字符是否为 ASCII 字母
(ascii-alphabetic? #\a)

;; 示例2：把 ASCII 字符串转换为大写
(ascii-upcase "hello")

;; 示例3：进行大小写无关的字符串比较
(ascii-string-ci=? "Hello" "hello")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/ascii "函数名"
```

## 函数分类索引

### 一、本库实现
- `ascii-left-paren?` - 判断是否为 ASCII 左括号
- `ascii-right-paren?` - 判断是否为 ASCII 右括号

### 二、重导出的 SRFI-175 ASCII 函数

以下函数由 `(srfi srfi-175)` 实现，对应单元测试位于 `tests/srfi/srfi-175/` 目录：

- 类型判定：`ascii-codepoint?`、`ascii-bytevector?`、`ascii-char?`、`ascii-string?`
- 字符分类：`ascii-control?`、`ascii-non-control?`、`ascii-whitespace?`、`ascii-space-or-tab?`、`ascii-other-graphic?`、`ascii-upper-case?`、`ascii-lower-case?`、`ascii-alphabetic?`、`ascii-alphanumeric?`、`ascii-numeric?`
- 字符转换：`ascii-digit-value`、`ascii-upper-case-value`、`ascii-lower-case-value`、`ascii-nth-digit`、`ascii-nth-upper-case`、`ascii-nth-lower-case`、`ascii-upcase`、`ascii-downcase`、`ascii-control->graphic`、`ascii-graphic->control`、`ascii-mirror-bracket`
- 大小写无关比较：`ascii-ci=?`、`ascii-ci<?`、`ascii-ci>?`、`ascii-ci<=?`、`ascii-ci>=?`、`ascii-string-ci=?`、`ascii-string-ci<?`、`ascii-string-ci>?`、`ascii-string-ci<=?`、`ascii-string-ci>=?`
