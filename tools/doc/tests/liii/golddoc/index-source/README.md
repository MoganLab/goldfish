# (liii golddoc index-source) 模块导览

`(liii golddoc index-source)` 扫描库加载根目录下的库源码，从
`(define-library ... (export ...))` 中抽取导出的函数名，生成“函数名 → 库名”索引。

扫描时跳过 `tests` 目录与 `*-test.scm` 文件，并且只收录库名首段与所在分组目录同名的库，
避免把安装目录下的 `library/`、`tools/` 等非库目录误记为库。

## 函数分类索引

### 索引生成
- `build-index-for-load-roots` - 扫描若干个加载根目录并合并导出的函数索引
- `index->json-value` - 把函数索引转换为可序列化为 JSON 的值
