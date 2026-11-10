# (liii path fs) 模块函数分类索引

`(liii path fs)` 提供与文件系统交互的路径操作函数，
包括存在性判断、读写文本与字节、目录创建删除、复制重命名等，
适合脚本化的文件管理场景。

## 常见用法示例

```scheme
(import (liii path))

;; 示例1：读写文本文件
(define p (path "out.txt"))
(path-write-text p "hello")
(path-read-text p)               ; => "hello"

;; 示例2：目录操作
(path-mkdir (path "sub/dir"))    ; 递归创建目录
(path-exists? (path "sub"))      ; => #t
(path-rmdir (path "sub/dir"))    ; 删除空目录

;; 示例3：列出目录内容
(path-list (path "."))           ; => 文件名向量
(path-list-path (path "."))      ; => 路径对象向量
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/fs "函数名"
```

## 函数分类索引

### 一、存在性与类型判断

- `path-exists?` - 判断路径是否存在
- `path-file?` - 判断是否为普通文件
- `path-dir?` - 判断是否为目录

### 二、读写操作

- `path-read-text` / `path-write-text` / `path-append-text` - 文本读写与追加
- `path-read-bytes` / `path-write-bytes` - 字节读写
- `path-read-lines` - 按行读取文本
- `path-getsize` - 获取文件大小

### 三、目录操作

- `path-mkdir` - 递归创建目录
- `path-rmdir` - 删除空目录
- `path-list` - 列出目录中的文件名（返回向量）
- `path-list-path` - 列出目录中的路径对象（返回向量）

### 四、文件管理

- `path-copy` - 复制文件到目标路径
- `path-copy-into` - 复制文件到目标目录
- `path-rename` - 重命名/移动文件
- `path-unlink` - 删除文件
- `path-touch` - 创建空文件或更新时间戳
