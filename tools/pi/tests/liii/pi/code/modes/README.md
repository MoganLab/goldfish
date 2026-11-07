# (liii pi code modes) 运行模式聚合测试

`(liii pi code modes)` 聚合重导出各运行模式的入口能力（print/json 模式、流程编排与应用模式解析）。

本目录存放跨子库的运行模式集成测试，各子库的单函数测试位于对应子目录。

使用时导入：

```scheme
(import (liii pi code modes))
```

## 子库导览

- `config/` - `gf pi config` 配置 TUI
- `output/` - print 模式结果输出
- `flow/` - Agent 会话流程编排
- `interactive/` - 交互式 TUI 模式
