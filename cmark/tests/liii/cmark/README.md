# (liii cmark) 模块导览

`(liii cmark)` 是 Goldfish Scheme 的 Markdown 解析与 AST / md-stem 转换库。

## 公开接口

- `markdown->md-stem` ：把 Markdown 文本解析为 md-stem S 表达式（正向转换）。
- `md-stem->markdown` ：把 md-stem S 表达式反向渲染为 Markdown 文本字符串。
  接收形如 `(md-stem 块 ...)` 的完整文档，返回以 `\n` 换行、顶层块以空行分隔的
  Markdown 字符串；空文档返回空字符串。链接统一输出内联形式，可自动链接化的
  链接（url 等于文本、或 url 为 `mailto:text`）输出 `<...>` 形式。未知或畸形
  节点抛出 `md-stem-error`，不静默丢弃内容。

## 双向转换与资源测试

`(liii cmark)` 提供双向转换：Markdown → md-stem（`markdown->md-stem`）与
md-stem → Markdown（`md-stem->markdown`）。成对资源端到端测试默认对每个资源
执行双向检查：

- `both`（默认）：`Markdown -> md-stem` 等于资源，且
  `md-stem -> Markdown -> md-stem` 等于资源（语义往返）。
- `forward-only`：只检查 `Markdown -> md-stem`。md-stem 未保留往返所需语义
  （如 HTML 实体、列表标记字符差异、`\[...\]` 与项目扩展数学块定界冲突、
  部分强调定界歧义等）。
- `reverse-only`：只检查 `md-stem -> Markdown -> md-stem`。

语义往返不等于逐字还原原始 Markdown：反向转换会规范化空白、统一链接与围栏
形式，md-stem 也不保留引用链接定义形式，因此验收以 md-stem 语义往返为准。
方向例外集中登记在 `e2e-directions.scm`，每条都引用具体资源路径并说明原因。

## 子模块导览

- `(liii cmark node)` - AST 节点记录、构造器与树结构操作
- `(liii cmark parser)` - 解析统一入口
- `(liii cmark block parser)` - 块级解析器
- `(liii cmark inline parser)` - 行内解析器
- `(liii cmark inline math)` - 数学公式解析器
- `(liii cmark stem)` - Markdown 到 md-stem S 表达式转换
- `(liii cmark markdown)` - md-stem S 表达式到 Markdown 文本反向转换
- `(liii cmark stem-reader)` - `.md.stem` 资源文本读取器
- `(liii cmark text)` - 通用字符串与文本辅助工具
