# (liii pi code core session-entry) 模块函数索引

`(liii pi code core session-entry)` 定义 JSONL 会话条目的 record 类型（header、message、compaction、context_edit、state），并提供成对的 `pi-session-entry->json` 与 `json->pi-session-entry` 转换接口。

使用时导入：

```scheme
(import (liii pi code core session-entry))
```

## 查看函数文档

```bash
gf doc json->pi-session-entry
```

## 函数分类索引

### 条目构造与访问
- `pi-session-header-entry` / `pi-session-header-entry?` - 会话头条目
- `pi-session-message-entry` / `pi-session-message-entry?` - 消息条目
- `pi-session-compaction-entry` / `pi-session-compaction-entry?` - 压缩条目
- `pi-session-context-edit-entry` / `pi-session-context-edit-entry?` - 上下文编辑条目
- `pi-session-state-entry` / `pi-session-state-entry?` - 状态条目

### 通用接口与 JSON 转换
- `pi-session-entry?` - 判断是否为会话条目 record
- `pi-session-entry-type` / `pi-session-entry-id` / `pi-session-entry-parent-id` / `pi-session-entry-timestamp` - 通用字段访问
- `pi-session-entry->json` - 会话条目 record 转 Scheme-JSON 对象
- `json->pi-session-entry` - Scheme-JSON 对象转会话条目 record
