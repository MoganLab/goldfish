# (liii pi code core compaction cut) 模块函数分类索引

`(liii pi code core compaction cut)` 提供会话条目解析与压缩切分点查找能力。

使用时导入：

```scheme
(import (liii pi code core compaction cut))
```

## 查看函数文档

```bash
gf doc pi-entry-message
gf doc pi-entry-id
gf doc pi-valid-cut-point?
gf doc pi-find-cut-point
gf doc pi-messages-to-summarize
```

## 函数分类索引

### 会话条目
- `pi-entry-message` - 读取会话条目中的消息对象
- `pi-entry-id` - 读取会话条目的 ID
- `pi-valid-cut-point?` - 判断会话条目是否可作为压缩切分点

### 切分点查找
- `pi-find-cut-point` - 在会话条目中查找合适的压缩切分点
- `pi-messages-to-summarize` - 提取切分点之前需要被总结的消息
