# (liii pi ai tools trunc-result) 模块函数索引

`(liii pi ai tools trunc-result)` 定义输出截断结果 record `<pi-trunc-result>` 及其构造器、谓词与访问器。

使用时导入：

```scheme
(import (liii pi ai tools trunc-result))
```

## 函数分类索引

- `pi-trunc-result` - 构造 `<pi-trunc-result>` 实例
- `pi-trunc-result?` - 判断是否为 `<pi-trunc-result>` 实例
- `pi-trunc-result-content` - 获取截断后的内容
- `pi-trunc-result-truncated` - 获取是否发生截断
- `pi-trunc-result-truncated?` - `pi-trunc-result-truncated` 别名
- `pi-trunc-result-truncated-by` - 获取截断触发维度
- `pi-trunc-result-total-lines` - 获取原始总行数
- `pi-trunc-result-total-bytes` - 获取原始总字节数
- `pi-trunc-result-output-lines` - 获取输出行数
- `pi-trunc-result-output-bytes` - 获取输出字节数
- `pi-trunc-result-first-line-exceeds-limit?` - 首行是否超限
- `pi-trunc-result-first-line-truncated` - `pi-trunc-result-first-line-exceeds-limit?` 别名
- `pi-trunc-result-last-line-partial?` - 末行是否不完整
- `pi-trunc-result-last-line-truncated` - `pi-trunc-result-last-line-partial?` 别名
