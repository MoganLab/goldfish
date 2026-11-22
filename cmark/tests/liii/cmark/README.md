# (liii cmark) 模块导览

`(liii cmark)` 是 Goldfish Scheme 的 Markdown 解析与 AST / md-stem 转换库。

## 子模块导览

- `(liii cmark node)` - AST 节点记录、构造器与树结构操作
- `(liii cmark parser)` - 解析统一入口
- `(liii cmark block parser)` - 块级解析器
- `(liii cmark inline parser)` - 行内解析器
- `(liii cmark inline math)` - 数学公式解析器
- `(liii cmark stem)` - Markdown 到 md-stem S 表达式转换
- `(liii cmark stem-reader)` - `.md.stem` 资源文本读取器
- `(liii cmark text)` - 通用字符串与文本辅助工具
