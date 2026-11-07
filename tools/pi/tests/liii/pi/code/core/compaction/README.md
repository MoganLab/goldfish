# (liii pi code core compaction) 会话压缩聚合测试

`(liii pi code core compaction)` 聚合重导出会话压缩的各个子库接口。

本目录存放跨子库的压缩流程集成测试（如压缩端到端流程），各子库的单函数测试位于对应子目录。

使用时导入：

```scheme
(import (liii pi code core compaction))
```

## 子库导览

- `tokens/` - 上下文 token 估算与压缩配置
- `cut/` - 会话切分点查找
- `fileops/` - 文件操作记录汇总
- `serialize/` - 会话序列化与文本提取
- `prompt/` - 压缩摘要提示词常量与构建
- `summary/` - 压缩摘要生成
- `session/` - 会话压缩记录读写
- `flow/` - 压缩流程编排
