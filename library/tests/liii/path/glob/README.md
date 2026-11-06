# (liii path glob) 模块函数分类索引

`(liii path glob)` 提供目录通配检索函数，
支持单层匹配、递归匹配与模式判断，适合文件筛选与批量处理场景。

## 常见用法示例

```scheme
(import (liii path glob))

;; 示例1：单层目录通配检索
(path-glob (path ".") "*.scm")     ; => 当前目录下 .scm 路径列表

;; 示例2：递归全目录树检索
(path-rglob (path "src") "*.md")   ; => 递归查找全部 .md 文件

;; 示例3：判断路径是否匹配模式
(path-match? (path "a/b.py") "*.py")   ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/glob "函数名"
```

## 函数分类索引

### 一、目录检索

- `path-glob` - 单层目录通配检索（返回路径对象列表）
- `path-rglob` - 递归全目录树通配检索（返回路径对象列表）

### 二、模式判断

- `path-match?` - 判断路径是否匹配通配模式
