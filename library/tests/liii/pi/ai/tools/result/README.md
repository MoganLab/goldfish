# (liii pi ai tools result) 模块函数索引

`(liii pi ai tools result)` 定义工具执行结果 record `<pi-tool-result>` 及其构造器、谓词、访问器与 JSON 序列化/反序列化函数。

使用时导入：

```scheme
(import (liii pi ai tools result))
```

## 函数分类索引

- `pi-tool-result` - 构造 `<pi-tool-result>` 实例
- `pi-tool-result?` - 判断是否为 `<pi-tool-result>` 实例
- `pi-tool-result-content` - 获取结果内容块向量
- `pi-tool-result-is-error` - 获取是否为错误结果
- `pi-tool-result-isError` - `pi-tool-result-is-error` 别名
- `pi-tool-result->json` - 将 `<pi-tool-result>` 转换为 JSON 对象
- `json->pi-tool-result` - 将 JSON 对象转换为 `<pi-tool-result>`
