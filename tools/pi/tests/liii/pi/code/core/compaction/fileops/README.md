# (liii pi code core compaction fileops) 模块函数分类索引

`(liii pi code core compaction fileops)` 提供压缩流程中的文件操作记录提取与汇总能力。

使用时导入：

```scheme
(import (liii pi code core compaction fileops))
```

## 查看函数文档

```bash
gf doc pi-extract-file-ops
gf doc pi-compute-file-lists
gf doc pi-format-file-operations
```

## 函数分类索引

### 文件操作记录
- `pi-extract-file-ops` - 从会话消息中提取文件操作记录
- `make-pi-file-ops` - 创建文件操作记录
- `pi-file-ops?` - 判断对象是否为文件操作记录
- `pi-file-ops-read` - 读取文件操作记录中的被读取文件列表
- `pi-file-ops-written` - 读取文件操作记录中的被写入文件列表
- `pi-file-ops-edited` - 读取文件操作记录中的被编辑文件列表

### 文件列表汇总
- `pi-compute-file-lists` - 根据文件操作记录汇总读取文件与修改文件列表
- `make-pi-file-lists` - 创建文件列表记录
- `pi-file-lists?` - 判断对象是否为文件列表记录
- `pi-file-lists-read-files` - 读取文件列表记录中的被读取文件列表
- `pi-file-lists-modified-files` - 读取文件列表记录中的被修改文件列表

### 展示
- `pi-format-file-operations` - 将文件操作汇总格式化为可读文本
