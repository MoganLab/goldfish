# (liii pi ai event record) 模块函数索引

`(liii pi ai event record)` 提供 Agent 事件的业务层构造器、事件类型提取函数及事件对象转 alist 的转换器。

使用时导入：

```scheme
(import (liii pi ai event record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-event-session
```

## 函数分类索引

### 事件构造器与类型
- `pi-event-session` / `pi-event-session-type` - 会话启动事件
- `pi-event-agent-start` / `pi-event-agent-start-type` - Agent 循环开始事件
- `pi-event-turn-start` / `pi-event-turn-start-type` - 单轮回合开始事件
- `pi-event-message-start` / `pi-event-message-start-type` - 消息开始事件
- `pi-event-message-update` / `pi-event-message-update-type` - 消息增量事件
- `pi-event-message-end` / `pi-event-message-end-type` - 消息结束事件
- `pi-event-tool-execution-start` / `pi-event-tool-execution-start-type` - 工具执行开始事件
- `pi-event-tool-execution-update` / `pi-event-tool-execution-update-type` - 工具执行更新事件
- `pi-event-tool-execution-end` / `pi-event-tool-execution-end-type` - 工具执行结束事件
- `pi-event-turn-end` / `pi-event-turn-end-type` - 单轮回合结束事件
- `pi-event-agent-end` / `pi-event-agent-end-type` - Agent 循环结束事件
- `pi-event-agent-settled` / `pi-event-agent-settled-type` - Agent 结算事件

### 通用函数
- `pi-event?` - 判断是否为事件对象
- `pi-event-type` - 获取事件类型字符串
- `pi-event->alist` - 事件对象转 alist
