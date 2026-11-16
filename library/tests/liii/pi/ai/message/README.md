# (liii pi ai message) 模块函数索引

`(liii pi ai message)` 是 Pi 消息与内容块（content block）的聚合入口，导出所有消息与块类型及其序列化、反序列化和多态判定接口。

按单 record 单文件规范，底层各类型也支持独立导入：
- `(liii pi ai message user)` - `<pi-user-message>`
- `(liii pi ai message assistant)` - `<pi-assistant-message>`
- `(liii pi ai message tool-result)` - `<pi-tool-result-message>`
- `(liii pi ai message text-block)` - `<pi-text-block>`
- `(liii pi ai message thinking-block)` - `<pi-thinking-block>`
- `(liii pi ai message tool-call-block)` - `<pi-tool-call-block>`
- `(liii pi ai message image-block)` - `<pi-image-block>`
- `(liii pi ai message block)` - 内容块多态接口

## 函数分类索引

- `pi-message?` - 判断是否为消息 record
- `pi-message-role` - 获取消息角色
- `pi-message->json` - 将消息 record 转换为 JSON 对象
- `json->pi-message` - 将 JSON 对象转换为消息 record
