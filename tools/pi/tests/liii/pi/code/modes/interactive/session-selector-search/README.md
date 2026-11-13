# (liii pi code modes interactive session-selector-search)

会话选择器搜索过滤与分层打分辅助库。

## 导出接口

- `make-session-search-cache`：创建用于缓存消毒并小写化后的会话搜索文本的哈希表。
- `session-selector-filter-sessions`：对会话列表执行 UTF-8 容错过滤与分层打分。
