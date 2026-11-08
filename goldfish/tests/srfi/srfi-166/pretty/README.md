# (srfi srfi-166 pretty) 模块函数分类索引

`(srfi srfi-166 pretty)` 在 SRFI-166 格式化组合子上实现
Scheme 代码美化排版：按关键字缩进惯例换行对齐，并可选
标注共享结构或加 ANSI 高亮。`(liii format pretty)` 以
Goldfish 命名重导出本库全部函数。

## 常见用法示例

```scheme
(import (srfi srfi-166 base)
        (srfi srfi-166 pretty))

(show #f (pretty '(foo bar)))         ; => "(foo bar)\n"
(show #f (pretty '(lambda (x) (+ x 1)))) ; => "(lambda (x) (+ x 1))\n"
(show #f (pretty-with-color '(let ((x 1)) x)))
; => "(\x1b;[34mlet\x1b;[39m ((x 1)) x)\n"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-166/pretty "函数名"
```

## 函数分类索引

### 一、基础排版
- `pretty` - 按 Scheme 惯例美化输出 S 表达式
- `pretty-with-color` - 在 pretty 基础上为关键字着色

### 二、共享结构处理
- `pretty-shared` - 标注共享与循环结构
- `pretty-simply` - 完全不标注共享结构

## 错误处理约定

- 四个函数都直接接收对象而非组合子，传入非对象参数无额外校验
- `pretty-simply` 遇到循环结构会无限展开，请先确认数据无环
