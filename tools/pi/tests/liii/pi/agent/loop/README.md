# (liii pi agent loop) 模块函数索引

`(liii pi agent loop)` 提供 Agent 对话主循环的驱动引擎与单步状态机，负责多轮工具调用循环、上下文压缩检查、系统提示词动态解析以及生命周期事件的派发。

使用时导入：

```scheme
(import (liii pi agent loop))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc run-agent-loop
```

## 函数分类索引

### 主循环驱动
- `run-agent-loop` - 执行 Agent 完整的多轮工具调用与事件驱动主循环
- `pi-step-agent-turn` - 执行单轮模型请求与响应解析，提取工具调用与停止原因

### 提示词与上下文
- `pi-resolve-turn-system-prompt` - 解析单轮系统提示词并动态注入预算与时间信息
- `pi-maybe-compact` - 检查上下文是否超出阈值并在需要时执行自动压缩

### 工具调用处理
- `pi-handle-tool-use` - 处理单轮模型输出的工具调用执行并更新消息历史

### 生命周期与事件
- `pi-finish-agent-turn` - 结束单轮对话并派发 turn_end 和扩展事件
- `pi-notify-turn-end` - 派发单轮结束事件 turn_end
- `pi-emit-agent-end` - 派发整个 Agent 会话结束事件 agent_end
- `pi-emit-agent-error` - 构造并派发 Agent 异常错误事件
