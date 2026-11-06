# (liii path pure) 模块函数分类索引

`(liii path pure)` 提供不访问文件系统的纯路径计算函数，
全部基于字符串与路径结构完成，适合路径解析、比较与改写等场景。

## 常见用法示例

```scheme
(import (liii path pure))

;; 示例1：判断路径是否绝对
(path-absolute? (path "/usr/bin"))    ; => #t

;; 示例2：拼接与解析路径
(path-join "/home" "user" "doc.md")   ; => #<path /home/user/doc.md>
(path-resolve (path "/a/b/../c"))     ; => #<path /a/c>

;; 示例3：相对化与父目录
(path-relative-to (path "/a/b/c") (path "/a"))  ; => #<path b/c>
(path-parent (path "/a/b/c"))                    ; => #<path /a/b>
(path-parents (path "/a/b/c"))                   ; => 逐级父目录列表
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/pure "函数名"
```

## 函数分类索引

### 一、路径属性判断

- `path-absolute?` - 判断是否为绝对路径
- `path-starts-with?` - 判断路径是否以另一路径开头

### 二、路径计算

- `path-join` - 拼接多个路径片段
- `path-resolve` - 折叠 `.` 与 `..` 规范化路径
- `path-relative-to` - 计算相对于另一路径的路径
- `path-parent` - 获取父目录路径
- `path-parents` - 获取逐级父目录列表

### 三、用户目录展开

- `path-expanduser` - 将 `~` 展开为用户主目录
