# (liii path record) 模块函数分类索引

`(liii path record)` 定义路径的核心记录类型 `<path>` 及其字段访问器，
并提供路径构造、分量读取与字符串互转等基础函数，
是 `(liii path)` 各子模块的底层数据表示。

## 常见用法示例

```scheme
(import (liii path)
        (liii path record))

;; 示例1：构造路径并读取记录字段
(define p (path "/a/b.txt"))
(path-record-parts p)            ; => #("a" "b.txt")
(path-record-type p)             ; => posix
(path-record-root p)             ; => #\/

;; 示例2：字段访问器与修改器
(path-record-set-parts! p #("c" "d.md"))
(path->string p)                 ; => "/c/d.md"

;; 示例3：anchor 与分量拼接
(anchor-string 'posix "" #\/)    ; => "/"
(parts->string #("a" "b") "/")   ; => "a/b"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/path/record "函数名"
```

## 函数分类索引

### 一、记录类型与字段

- `make-path-record` - 直接构造路径记录对象
- `path?` - 判断对象是否为路径记录
- `path-record-parts` / `path-record-set-parts!` - 分量向量字段
- `path-record-type` / `path-record-set-type!` - 路径类型字段
- `path-record-drive` / `path-record-set-drive!` - 盘符字段
- `path-record-root` / `path-record-set-root!` - 根分隔符字段

### 二、底层拼接工具

- `anchor-string` - 构造路径 anchor 首元素字符串
- `parts->string` - 用分隔符拼接分量向量

### 三、构造函数

- `path` - 从字符串或其他 path 创建路径对象
- `path-from-string` - 从字符串创建路径
- `path-from-parts` - 从分量向量创建路径
- `path-of-drive` - 创建 Windows 驱动器路径
- `path-from-env` - 从环境变量创建路径
- `path-cwd` - 获取当前工作目录路径
- `path-home` - 获取用户主目录路径
- `path-temp-dir` - 获取临时目录路径

### 四、属性访问与判断

- `path-parts` - 获取路径分量（含 anchor）
- `path-type` - 获取路径类型
- `path-drive` - 获取盘符
- `path-root` - 获取根分隔符
- `path-as-posix` - 按 posix 语义解析字符串
- `path-absolute?` / `path-relative?` - 判断路径绝对性
- `path-equals?` / `path=?` - 判断两路径相等

### 五、转换

- `path->string` - 将路径转换为字符串
