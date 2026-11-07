# (liii pi code core compaction serialize) 模块函数分类索引

`(liii pi code core compaction serialize)` 提供会话内容序列化与文本提取能力。

使用时导入：

```scheme
(import (liii pi code core compaction serialize))
```

## 查看函数文档

```bash
gf doc pi-content->text
gf doc pi-truncate-for-summary
gf doc pi-serialize-conversation
```

## 函数分类索引

### 序列化
- `pi-content->text` - 提取消息内容中的全部文本并拼接为字符串
- `pi-truncate-for-summary` - 按长度上限截断用于摘要的文本
- `pi-serialize-conversation` - 将会话消息列表序列化为纯文本
