# (liii pi ai event types) 模块函数索引

`(liii pi ai event types)` 提供 AI 交互和 Agent 运行时事件的底层结构体定义与访问器。

使用时导入：

```scheme
(import (liii pi ai event types))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-event-session?
```

## 函数分类索引

### 会话与 Agent 事件类型
- `pi-event-session?` - 会话启动事件谓词及访问器
- `pi-event-agent-start?` - Agent 循环启动事件谓词
- `pi-event-turn-start?` - 单轮回合开始事件谓词
- `pi-event-message-start?` - 消息开始事件谓词及访问器
- `pi-event-message-update?` - 消息增量事件谓词及访问器
- `pi-event-message-end?` - 消息结束事件谓词及访问器
- `pi-event-tool-execution-start?` - 工具调用开始事件谓词及访问器
- `pi-event-tool-execution-update?` - 工具调用增量事件谓词及访问器
- `pi-event-tool-execution-end?` - 工具调用结束事件谓词及访问器
- `pi-event-turn-end?` - 单轮回合结束事件谓词及访问器
- `pi-event-agent-end?` - Agent 循环结束事件谓词及访问器
- `pi-event-agent-settled?` - Agent 任务最终结算事件谓词及访问器
