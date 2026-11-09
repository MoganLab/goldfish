# (srfi 166) 模块函数分类索引

SRFI-166 提供「漂亮打印」（pretty-printing）能力：`show`、`written`、
`fitted`、`trimmed`、`padded`、`joined` 等过程把数据渲染成适合阅读的排版结果。

`(srfi 166)` 重导出 `(srfi srfi-166)` 的全部接口，
并按能力拆分到多个子目录：

| 子目录 | 内容 |
| --- | --- |
| `tests/srfi/srfi-166/base/` | 基础渲染过程（`show`、`written`、`each`、`joined`、`padded` 等） |
| `tests/srfi/srfi-166/pretty/` | 漂亮打印过程（`pretty`、`pretty-shared` 等） |
| `tests/srfi/srfi-166/columnar/` | 分栏与表格排版（`columnar`、`tabular`、`wrapped`、`justified` 等） |
| `tests/srfi/srfi-166/color/` | 颜色与样式（`as-red`、`as-bold`、`on-blue` 等） |
| `tests/srfi/srfi-166/unicode/` | Unicode 宽度感知（`terminal-aware`、`substring-terminal-width` 等） |

## 常见用法示例

```scheme
(import (srfi 166))

;; 示例1：漂亮打印
(pretty '(1 2 3))

;; 示例2：带颜色的输出
(pretty-with-color '(1 2 3))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/166 "函数名"
```
