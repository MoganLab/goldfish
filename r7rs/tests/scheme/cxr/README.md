# (scheme cxr) 模块函数分类索引

`(scheme cxr)` 提供 R7RS 的复合 car/cdr 访问器。
这些函数由底层 S7 直接提供，本库负责按 R7RS 规范导出，
适合读写列表嵌套结构时替代层层嵌套的 `car` 与 `cdr`。

## 常见用法示例

```scheme
(import (scheme cxr))

;; 示例1：三层嵌套列表的分量访问
(caar '((1) (2)))              ; => 1
(cadr '((1) (2)))              ; => (2)
(caddr '(1 2 3))               ; => 3

;; 示例2：四层访问器
(cadaddr '(1 2 (3 4)))         ; => 4
(cddddr '(1 2 3 4 5 6))        ; => (6)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/cxr "函数名"
```

## 函数分类索引

### 一、三层访问器

由 `caar` 到 `cdddr` 的 8 个函数

- `caaar` `caadr` `cadar` `caddr` - 以 `a`（car）或 `d`（cdr）组合访问
- `cdaar` `cdadr` `cddar` `cdddr` - 以 `d`（cdr）开头的组合访问

### 二、四层访问器

由 `caaaar` 到 `cddddr` 的 16 个函数

- `caaaar` `caaadr` `caadar` `caaddr` - 以 `caa` 开头的组合访问
- `cadaar` `cadadr` `caddar` `cadddr` - 以 `cad` 开头的组合访问
- `cdaaar` `cdaadr` `cdadar` `cdaddr` - 以 `cda` 开头的组合访问
- `cddaar` `cddadr` `cdddar` `cddddr` - 以 `cdd` 开头的组合访问
