# (srfi srfi-39) 模块函数分类索引

SRFI-39 提供「参数对象」（parameter object）机制：
`make-parameter` 创建一个携带当前值的零参数过程，
`parameterize` 在动态作用域内临时替换参数值，退出后自动恢复。

## 常见用法示例

```scheme
(import (srfi srfi-39))

;; 示例1：创建参数对象
(define param (make-parameter "initial value"))
(param) ; => "initial value"

;; 示例2：临时替换参数值
(parameterize ((param "new value")) (param)) ; => "new value"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-39 "函数名"
```

## 函数分类索引

### 一、参数对象
- `make-parameter` - 创建参数对象，返回读取当前值的零参数过程
- `parameterize` - 在动态作用域内临时替换参数对象的值
