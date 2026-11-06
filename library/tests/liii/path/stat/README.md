# (liii path stat) 模块函数分类索引

`(liii path stat)` 定义 `<path-stat>` 文件元数据记录类型和相关的属性访问器与类型判断谓词。
这是 `(liii path)` 的子模块，也可单独导入使用。

## 常见用法示例

```scheme
(import (liii path stat))

;; 示例1：获取文件元数据并读取属性
(define st (path-stat "README.md"))
(path-stat? st)                  ; => #t
(path-stat-size st)              ; => 文件字节大小
(path-stat-mtime st)             ; => 最后修改时间（秒）
(path-stat-file? st)             ; => #t
(path-stat-dir? st)              ; => #f
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc liii/path/stat "path-stat"
gf doc liii/path/stat "path-stat-size"
```

## 函数分类索引

### 一、构造函数与记录类型谓词

- `make-path-stat` - 构造 `<path-stat>` 元数据记录对象
- `path-stat` - 获取文件或目录的元数据记录对象
- `path-stat?` - 判断是否为 `<path-stat>` 记录对象

### 二、元数据属性访问器

- `path-stat-size` - 获取文件字节大小
- `path-stat-mtime` - 获取最后修改时间（秒）
- `path-stat-atime` - 获取最后访问时间（秒）
- `path-stat-ctime` - 获取状态改变或创建时间（秒）
- `path-stat-mode` - 获取文件模式与权限位

### 三、文件类型判定谓词

- `path-stat-file?` - 基于元数据判断是否为普通文件
- `path-stat-dir?` - 基于元数据判断是否为目录
- `path-stat-symlink?` - 基于元数据判断是否为符号链接
