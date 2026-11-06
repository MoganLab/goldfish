# (liii path parse) 模块函数分类索引

`(liii path parse)` 提供路径字符串的底层解析函数，
负责把 posix 与 Windows 路径切分为段向量、盘符与根锚点，
供 `(liii path record)` 等上层模块构造路径记录使用。

## 常见用法示例

```scheme
(import (liii path parse)
        (srfi srfi-11))

;; 示例1：解析 posix 普通路径
(let-values (((parts root) (parse-path-string "/a/b")))
  (list parts root))            ; => (#("a" "b") #\/)

;; 示例2：解析 Windows 路径
(let-values (((parts drive root) (parse-windows-path "C:\\a\\b")))
  (list parts drive root))      ; => (#("a" "b") "C" #\\)

;; 示例3：识别 UNC 前缀与盘符
(unc-prefix? "\\\\server")      ; => #t
(windows-path-with-drive? "C:x") ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/parse "函数名"
```

## 函数分类索引

### 一、字符串切分

- `string-split-vec` - 按分隔符切分字符串为向量（保留空段）
- `drop-dot-parts` - 过滤 "." 段与空段
- `clean-tail` - 过滤 anchor 之后的空段与分隔符残段

### 二、前缀判断与盘符

- `unc-prefix?` - 判断是否以 UNC 前缀（\\）开头
- `windows-path-with-drive?` - 判断是否以盘符开头
- `extract-drive` - 提取大写盘符字母

### 三、路径解析

- `parse-path-string` - 解析 posix/普通路径为段向量与根
- `parse-unc` - 解析 UNC 路径为段向量、盘符与根
- `parse-windows-path` - 解析 Windows 路径为段向量、盘符与根
