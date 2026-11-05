# (liii pi ai api anthropic-messages thinking) 模块函数索引

`(liii pi ai api anthropic-messages thinking)` 负责 Anthropic 协议的思考（thinking）支持：
把思考级别映射为 token 预算、向请求体注入 thinking 配置，并在流式解析过程中
维护 thinking 块的开始、增量与结束。

流状态由 `(liii pi ai api anthropic-messages stream-state)` 提供。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages thinking))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-inject-thinking
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/thinking "anthropic-inject-thinking"
```

## 函数分类索引

### 思考级别与请求体
- `anthropic-thinking-level->budget` - 将 Anthropic 思考级别映射为对应的 thinking 预算 token 数
- `anthropic-inject-thinking` - 按思考级别向 Anthropic 请求体注入 thinking 配置

### 流式 thinking 块
- `anthropic-start-thinking-block` - 开始一个新的 Anthropic thinking 块，重置当前 thinking 状态并派发 thinking_start 事件
- `anthropic-handle-thinking-delta` - 处理 Anthropic thinking 或 signature 增量，并更新当前 thinking 状态
- `anthropic-finish-thinking-block` - 结束当前 Anthropic thinking 块，将累积内容写入消息块列表

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/thinking/
```

## 测试文件索引

- `anthropic-thinking-level-to-budget-test.scm` — `anthropic-thinking-level->budget`：各思考级别的预算映射
- `anthropic-inject-thinking-test.scm` — `anthropic-inject-thinking`：thinking 配置注入与 max_tokens 提升
- `anthropic-start-thinking-block-test.scm` — `anthropic-start-thinking-block`：thinking 状态建立与事件派发
- `anthropic-handle-thinking-delta-test.scm` — `anthropic-handle-thinking-delta`：thinking 与签名增量累积
- `anthropic-finish-thinking-block-test.scm` — `anthropic-finish-thinking-block`：thinking 块落盘与事件派发
