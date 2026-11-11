# (liii pi code all-in-one boundary) 模块函数分类索引

`(liii pi code all-in-one boundary)` 在跨模型交接前清洗会话尾部：回滚报错、中止、截断、空内容或悬空调用的 assistant 消息，保留用户消息、完整回复和已经执行完毕的 tool result。

使用时导入：

```scheme
(import (liii pi code all-in-one boundary))
```

## 查看函数文档

```bash
gf doc pi-boundary-recover-messages
gf doc pi-boundary-assistant-usable?
gf doc pi-boundary-message-role
gf doc pi-boundary-message-stop-reason
gf doc pi-boundary-message-content
```

## 函数分类索引

- `pi-boundary-recover-messages` - 回滚会话尾部不完整 assistant 消息
- `pi-boundary-assistant-usable?` - 判断 assistant 消息是否可安全交接
- `pi-boundary-message-role` - 统一读取消息记录或 alist 的 role
- `pi-boundary-message-stop-reason` - 统一读取 assistant 的 stopReason
- `pi-boundary-message-content` - 将字符串、向量或块列表统一为内容块列表
