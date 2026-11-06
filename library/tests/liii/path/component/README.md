# (liii path component) 模块函数分类索引

`(liii path component)` 提供路径末段分量的解析与访问函数，
包括文件名、主名、后缀的读取与替换，全部为纯计算、不访问文件系统。

## 常见用法示例

```scheme
(import (liii path component))

;; 示例1：读取末段分量
(path-name (path "/a/file.tar.gz"))    ; => "file.tar.gz"
(path-stem (path "/a/file.tar.gz"))    ; => "file.tar"
(path-suffix (path "/a/file.tar.gz"))  ; => ".gz"
(path-suffixes (path "/a/file.tar.gz")) ; => (".tar" ".gz")

;; 示例2：替换末段分量
(path-with-name (path "/a/b.txt") "c.md")    ; => #<path /a/c.md>
(path-with-stem (path "/a/b.txt") "c")       ; => #<path /a/c.txt>
(path-with-suffix (path "/a/b.txt") ".md")   ; => #<path /a/b.md>
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/component "函数名"
```

## 函数分类索引

### 一、分量访问

- `path-name` - 获取末段全名
- `path-stem` - 获取主名（不含最后后缀）
- `path-suffix` - 获取最后一个后缀（含点）
- `path-suffixes` - 获取全部后缀列表

### 二、分量替换

- `path-with-name` - 替换整个末段名字
- `path-with-stem` - 替换主名并保留后缀
- `path-with-suffix` - 替换最后一个后缀

### 三、内部工具

- `split-name-dots` - 解析末段的点分隔结构
- `replace-last-segment` - 替换路径末段为新名字
