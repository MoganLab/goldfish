# (srfi srfi-166) 模块函数分类索引

`(srfi srfi-166)` 是 SRFI-166 Monadic Formatting 的兼容性总入口：
一次性导出 base、color、columnar、pretty、unicode 五个子库的全部
组合子，`(srfi 166)` 是它的别名。日常使用建议按需只导入子库，
Goldfish 用户更推荐对应的 `(liii format ...)` 命名。

## 常见用法示例

```scheme
(import (srfi srfi-166 base)
        (srfi srfi-166 color))

(show #f "hello " "world")   ; => "hello world"
(show #f (as-red "red"))     ; => "\x1b;[31mred\x1b;[39m"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定子库名：
```bash
gf doc srfi/srfi-166/base "函数名"
```

## 子库与测试目录

| 子库 | 职责 | 导览与测试 |
| --- | --- | --- |
| `(srfi srfi-166 base)` | 组合子运行时：show、输出、填充、裁剪、数值格式 | `tests/srfi/srfi-166/base/README.md` |
| `(srfi srfi-166 color)` | ANSI 前景色、背景色与字符属性 | `tests/srfi/srfi-166/color/README.md` |
| `(srfi srfi-166 columnar)` | 多列排版、折行、行号、按行读文件 | `tests/srfi/srfi-166/columnar/README.md` |
| `(srfi srfi-166 pretty)` | S 表达式美化排版与高亮 | `tests/srfi/srfi-166/pretty/README.md` |
| `(srfi srfi-166 unicode)` | 终端显示宽度感知与大小写组合子 | `tests/srfi/srfi-166/unicode/README.md` |

## 错误处理约定

- 本库不定义新过程，错误处理沿用各子库的约定
- 只需要部分能力时不要导入本库，避免把五个子库的导出全部带入作用域
