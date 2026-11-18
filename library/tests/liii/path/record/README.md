# (liii path record) 模块函数分类索引

`(liii path record)` 提供路径构造、类型谓词、分量读取与字符串互转等基础函数。

## 路径类型说明

每个 path 对象由四个字段构成：`parts`（分量向量）、`type`（平台类型）、
`drive`（盘符）、`root`（根分隔符）。其中 `type` 分为两种，Windows 下再按
`drive`/`root` 的组合细分为五类（对齐 Python pathlib 的 PureWindowsPath 语义）：

### 平台类型（path-type 返回值）

- `'posix` — 以 `/` 为分隔符，无盘符概念；非 Windows 平台上所有路径都是 posix
- `'windows` — 以 `\` 为分隔符，支持盘符与 UNC；仅在 Windows 平台上，
  含盘符或 `\\` 前缀的字符串才识别为 windows（如 `(path "C:/a")`、
  `(path "\\\\srv\\sh")`），Windows 平台上 `(path "/")` 也会规范化为
  windows `"\"`。其他平台一律按 posix 处理，但 `path-of-drive`、
  `path-from-parts` 显式传入盘符/UNC anchor 时构造的仍是 windows 路径

### Windows 路径细分（按 drive 与 root 组合）

| 类别 | 示例 | drive | root | 绝对路径? |
| --- | --- | --- | --- | --- |
| 盘符绝对（drive-absolute） | `C:\foo` | `"C"` | `#\\` | 是 |
| 盘符相对（drive-relative） | `C:foo` | `"C"` | `#f` | 否（相对 C 盘当前目录） |
| UNC | `\\server\share\foo` | `"\\server\share"` | `#\\` | 是 |
| 当前驱动器根 | `\foo` | `""` | `#\\` | 否 |
| 相对路径 | `foo\bar` | `""` | `#f` | 否 |

### posix 路径细分

| 类别 | 示例 | root | 绝对路径? |
| --- | --- | --- | --- |
| 绝对路径 | `/foo/bar` | `#\/` | 是 |
| 相对路径 | `foo/bar` | `#f` | 否 |

判定规则：posix 只看 `root` 是否非空；Windows 要求 `drive` 与 `root`
同时非空才算绝对路径（所以 `C:\foo`、UNC 绝对，`C:foo`、`\foo` 都不是）。

`root` 字段取值 `#\\` / `#\/` 表示有根分隔符，`#f` 表示无根，用于区分
`C:\foo`（有根）与 `C:foo`（无根）的语义差异。

## 常见用法示例

```scheme
(import (liii path)
        (liii path record))

;; 示例1：构造路径并读取属性
(define p (path "/a/b.txt"))
(path? p)                        ; => #t
(path-parts p)                   ; => #("/" "a" "b.txt")
(path-type p)                    ; => posix
(path-root p)                    ; => "/"
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

### 一、构造函数

用于创建 path 对象的函数

- `path` - 从字符串或其他 path 创建路径对象
- `path-from-parts` - 从分量向量创建路径
- `path-of-drive` - 创建 Windows 驱动器路径
- `path-from-env` - 从环境变量创建路径
- `path-cwd` - 获取当前工作目录路径
- `path-home` - 获取用户主目录路径
- `path-temp-dir` - 获取临时目录路径

### 二、谓词

- `path?` - 判断对象是否为路径记录
- `path-absolute?` / `path-relative?` - 判断路径绝对性
- `path-equals?` / `path=?` - 判断两路径相等

### 三、属性访问

- `path-parts` - 获取路径分量（含 anchor）
- `path-type` - 获取路径类型（'posix 或 'windows，详见上方"路径类型说明"）
- `path-drive` - 获取盘符
- `path-root` - 获取根分隔符

### 四、转换

- `path->string` - 将路径转换为字符串
- `path-as-posix` - 将路径转换为以 / 为分隔符的字符串（对齐 pathlib 的 as_posix()；posix 下等价于 path->string，Windows 下把 \\ 替换为 /）
