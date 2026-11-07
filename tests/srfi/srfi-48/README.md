# (srfi srfi-48) 模块函数分类索引

SRFI-48 提供 `format` 过程：把格式串中的占位符替换为对应参数，
生成格式化后的字符串。

`(srfi srfi-48)` 重导出 `(liii base)` 提供的 `format` 实现。

## 常见用法示例

```scheme
(import (srfi srfi-48))

;; 示例1：格式化字符串
(format "~a-~a" 'x 'y) ; => "x-y"

;; 示例2：格式化数字
(format "~a" 42) ; => "42"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-48 "函数名"
```

## 函数分类索引

### 一、格式化
- `format` - 按格式串生成格式化字符串（由 `(liii base)` 实现，测试位于 `tests/liii/base/`）
