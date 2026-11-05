# (liii pi code modes flow) 模块函数索引

`(liii pi code modes flow)` 负责非交互（脚本）模式的会话流程：构造系统提示词、
落盘首轮用户消息、以解析出的超时驱动 Agent 主循环，并以 JSON 行输出事件
（对齐上游 pi 的非交互运行模式）。

使用时导入：

```scheme
(import (liii pi code modes flow))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 提示词与落盘
- `prepare-system-prompt` - 构造系统提示词（合并 CLI 覆盖、追加提示与项目上下文）
- `persist-initial-user-msg!` - 将首轮用户消息追加落盘到会话文件

### Agent 循环与事件输出
- `run-agent-loop-with-timeout` - 以解析出的超时与轮数上限驱动 Agent 主循环
- `emit-json-event` - 将事件序列化为 JSON 行写入标准输出

### 会话流程
- `run-session-flow` - 单会话模式运行一轮对话并回调结束处理
- `run-compact-flow` - 手动压缩最近一次会话（可继续本轮请求）
