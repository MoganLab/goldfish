# (liii base) 模块函数分类索引

`(liii base)` 是 `(scheme base)` 的扩展库，在 R7RS `scheme base` 的基础上
增补了来自 SRFI-2、SRFI-8 的实用函数，以及若干 S7 扩展工具。

如需使用 R7RS 标准函数，请查看：

```bash
gf doc scheme/base
```

## 常见用法示例

```scheme
(import (liii base))

;; 示例1：组合多个函数
((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3) ; => 8

;; 示例2：宽松地取 car
(loose-car '()) ; => ()
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/base "函数名"
```

## 函数分类索引

### 一、SRFI-2 条件绑定
- `and-let*` - 带条件的 let* 绑定（由 `(srfi srfi-2)` 实现，测试位于 `tests/srfi/srfi-2/`）

### 二、SRFI-8 多值处理
- `receive` - 接收多个返回值（由 `(srfi srfi-8)` 实现，测试位于 `tests/srfi/srfi-8/`）

### 三、S7 扩展
- `copy` - 复制序列或对象
- `define*` - 带默认参数和可选参数的 define
- `define-macro` - 定义宏
- `define-bacro` - 定义展开式宏
- `lambda*` - 带默认参数和可选参数的 lambda
- `procedure-source` - 获取过程的源代码定义
- `procedure-arglist` - 获取过程的参数列表
- `arity` - 获取过程可接受的参数数量范围
- `defined?` - 检查符号是否已定义
- `object->string` - 将对象转换为字符串表示
- `eval-string` - 将字符串作为 Scheme 代码求值
- `signature` - 获取函数的类型签名
- `keyword?` - 判断是否为关键字
- `string->keyword` - 字符串转关键字
- `symbol->keyword` - 符号转关键字
- `keyword->symbol` - 关键字转符号

### 四、实用工具函数
- `loose-car` - 宽松地获取 car（空列表返回空列表）
- `loose-cdr` - 宽松地获取 cdr（空列表返回空列表）
- `compose` - 组合多个函数
- `identity` - 原样返回输入值
- `any?` - 任意值谓词，恒返回 #t
- `make-hook` / `hook-functions` / `with-output-to-string` / `with-input-from-string` / `call-with-input-string` / `call-with-output-string` - 钩子与字符串端口辅助函数
- `format` - 格式化输出
- `cyclic-sequences` - 构造循环序列

### 五、列表操作
- `reverse!` - 原地反转列表（性能优于 reverse）
- `sort!` - 原地排序序列
