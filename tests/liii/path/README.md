# (liii path) 模块函数分类索引

`(liii path)` 是一个跨平台的路径抽象，支持 POSIX 和 Windows 两种路径格式。
与直接拼接字符串相比，它更适合表达路径层级、父目录和文件系统操作。

本库按职责拆分为多个子库：`(liii path pure)` 纯字符串路径计算、
`(liii path record)` 路径记录、`(liii path component)` 分量访问、
`(liii path stat)` 与 `(liii path fs)` 文件系统查询、`(liii path glob)` 通配检索。

## 常见用法示例

```scheme
(import (liii path))

;; 示例1：创建路径并获取信息
(define p (path "/home/user/file.txt"))
(path->string p)                 ; => "/home/user/file.txt"
(path-name p)                    ; => "file.txt"
(path-stem p)                    ; => "file"
(path-suffix p)                  ; => "txt"
(path-parent p)                  ; => #<path /home/user>

;; 示例2：路径拼接
(path-join "/home/user" "Documents" "file.txt")
;; => #<path /home/user/Documents/file.txt>

;; 示例3：检查路径属性
(path-absolute? (path "/usr/bin"))   ; => #t
(path-relative? (path "./file.txt")) ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc liii/path "path?"
gf doc liii/path "path-join"
```

## 函数分类索引

### 一、构造函数

用于创建 path 对象的函数

- `path` - 从字符串或其他 path 创建路径对象
- `path-from-string` - 从字符串创建路径（path 的别名）
- `path-from-parts` - 从路径各部分（向量）创建路径
- `path-of-drive` - 创建 Windows 驱动器路径
- `path-root` - 创建根路径 "/"
- `path-cwd` - 获取当前工作目录路径
- `path-home` - 获取用户主目录路径
- `path-temp-dir` - 获取临时目录路径
- `path-from-env` - 从环境变量创建路径

### 二、谓词函数

用于判断路径类型和属性的函数

- `path?` - 判断是否为 path 对象
- `path-absolute?` - 判断是否为绝对路径
- `path-relative?` - 判断是否为相对路径
- `path-exists?` - 判断路径是否存在
- `path-dir?` - 判断是否为目录
- `path-file?` - 判断是否为文件
- `path-equals?` - 判断两个路径是否相等
- `path=?` - path-equals? 的别名

### 三、属性访问

用于获取路径各组成部分的函数

- `path-parts` - 获取路径各部分（向量）
- `path-type` - 获取路径类型（'posix 或 'windows）
- `path-drive` - 获取 Windows 驱动器字母
- `path-name` - 获取文件名（最后一部分）
- `path-stem` - 获取文件名（不含扩展名）
- `path-suffix` - 获取文件扩展名

### 四、路径操作

用于操作和转换路径的函数

- `path-copy` - 复制文件到目标路径
- `path-copy-into` - 复制文件到目标目录
- `path-join` - 拼接多个路径片段
- `path-parent` - 获取父目录路径
- `path->string` - 将路径转换为字符串

### 五、目录列表操作

用于列出目录内容的函数

- `path-list` - 列出目录中的文件名（返回向量）
- `path-list-path` - 列出目录中的路径对象（返回向量）
- `path-glob` - 单层目录通配检索（返回路径对象列表）
- `path-rglob` - 递归全目录树通配检索（返回路径对象列表）
