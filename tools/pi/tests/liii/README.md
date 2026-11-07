# (liii pi) 工具测试根目录

gf pi 工具（Pi Coding Agent）的测试根目录：语言层 `(liii pi ...)` 库的单函数测试位于 `pi/` 子目录，CLI 与 Agent 级集成测试（端到端、多轮交互、队列状态机等）直接放在本目录。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 目录索引

- `pi/` - `(liii pi ...)` 各子库的单函数单元测试
- `pi-*-e2e-test.scm` - CLI / JSON 协议端到端集成测试
- `pi-agent-test.scm` 等 - Agent 工作流聚合测试
