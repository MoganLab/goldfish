# (liii pi code core compaction prompt) 模块函数分类索引

`(liii pi code core compaction prompt)` 提供压缩摘要提示词常量与提示词构建能力。

使用时导入：

```scheme
(import (liii pi code core compaction prompt))
```

## 查看函数文档

```bash
gf doc PI_SUMMARIZATION_PROMPT
gf doc pi-build-summarization-prompt
gf doc pi-compaction-summary-message
```

## 函数分类索引

### 提示词常量
- `PI_COMPACTION_SUMMARY_PREFIX` - 压缩摘要前缀常量
- `PI_COMPACTION_SUMMARY_SUFFIX` - 压缩摘要后缀常量
- `PI_SUMMARIZATION_SYSTEM_PROMPT` - 摘要生成系统提示词常量
- `PI_SUMMARIZATION_PROMPT` - 摘要生成提示词常量
- `PI_UPDATE_SUMMARIZATION_PROMPT` - 摘要增量更新提示词常量

### 提示词构建
- `pi-compaction-summary-message` - 将压缩摘要包装为会话消息
- `pi-build-summarization-prompt` - 构建会话压缩的摘要生成提示词
