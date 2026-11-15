# (liii pi code config types) 模块函数分类索引

`(liii pi code config types)` 定义用户自定义模型配置条目的 `<pi-model-entry>` 记录类型及其 JSON 序列化与反序列化接口。

使用时导入：

```scheme
(import (liii pi code config types))
```

## 函数分类索引

### 模型配置条目 (<pi-model-entry>)
- `make-pi-model-entry` - 构造 `<pi-model-entry>` 配置记录
- `pi-model-entry?` - 判断对象是否为 `<pi-model-entry>` 记录
- `pi-model-entry-id` - 读取模型 ID
- `pi-model-entry-name` - 读取模型展示名称
- `pi-model-entry-base-url` - 读取模型 baseUrl
- `pi-model-entry-api` - 读取模型 api 协议
- `pi-model-entry-reasoning` - 读取推理能力开关
- `pi-model-entry-context-window` - 读取上下文窗口大小
- `pi-model-entry-max-tokens` - 读取最大输出 token 数
- `pi-model-entry-extras` - 读取扩展字段 alist
- `json->pi-model-entry` - 将 JSON 对象转换为 `<pi-model-entry>` 记录
- `pi-model-entry->json` - 将 `<pi-model-entry>` 记录转换为 JSON 对象
