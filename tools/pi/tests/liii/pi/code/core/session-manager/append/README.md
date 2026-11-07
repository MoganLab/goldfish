# (liii pi code core session-manager append) 模块函数分类索引

`(liii pi code core session-manager append)` 提供会话条目追加与消息 ID 查找能力。

使用时导入：

```scheme
(import (liii pi code core session-manager append))
```

## 查看函数文档

```bash
gf doc append-session-entry!
gf doc find-last-session-message-id
gf doc append-session-state-entry!
```

## 函数分类索引

### 会话条目追加
- `append-session-entry!` - 追加一条会话条目到会话文件
- `append-session-state-entry!` - 向会话文件追加一条状态记录条目
- `find-last-session-message-id` - 查找会话文件中最后一条消息条目的 ID
