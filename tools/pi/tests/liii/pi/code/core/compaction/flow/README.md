# (liii pi code core compaction flow) 模块函数索引

`(liii pi code core compaction flow)` 负责自动压缩的编排：构造压缩摘要、
按配置生成自动压缩选项（compactor），并在压缩完成后把上下文标记为未知
（对齐上游 pi 的 compaction 流程）。

使用时导入：

```scheme
(import (liii pi code core compaction flow))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 摘要输出
- `pi-format-compaction-summary` - 将压缩结果格式化为一行摘要（压缩条数、保留条数、压缩前 Token 数）

### 自动压缩选项
- `pi-make-compaction-option` - 构造自动压缩选项（未启用时返回 #f）
