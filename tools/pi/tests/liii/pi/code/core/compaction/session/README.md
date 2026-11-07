# (liii pi code core compaction session) 模块函数分类索引

`(liii pi code core compaction session)` 提供会话压缩记录的读写与压缩流程执行能力。

使用时导入：

```scheme
(import (liii pi code core compaction session))
```

## 查看函数文档

```bash
gf doc pi-read-session-entries
gf doc pi-last-compaction-summary
gf doc pi-append-compaction-entry!
gf doc pi-compact-session!
```

## 函数分类索引

### 会话条目读取
- `pi-read-session-entries` - 读取会话文件中的全部条目
- `pi-last-compaction-summary` - 读取会话条目中最近一次压缩的摘要

### 压缩执行
- `pi-append-compaction-entry!` - 向会话文件追加一条压缩记录条目
- `pi-compact-session!` - 对会话文件执行一次压缩流程
