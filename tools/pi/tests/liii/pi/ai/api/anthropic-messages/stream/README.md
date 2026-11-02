# (liii pi ai api anthropic-messages stream) 模块函数索引

`(liii pi ai api anthropic-messages stream)` 负责 Anthropic Messages API 的流式请求：
拼装请求体、发起 SSE 流式请求，并把流式事件解析为内部 assistant 消息。
请求体拼装与端点构造由 `(liii pi ai api anthropic-messages request)` 提供，
流式事件解析与状态维护分别由 `(liii pi ai api anthropic-messages stream-parse)`
和 `(liii pi ai api anthropic-messages stream-state)` 提供。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages stream))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-request-stream
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/stream "anthropic-request-stream"
```

## 函数分类索引

### 流式请求
- `anthropic-request-stream` - 向 Anthropic Messages API 发起流式请求，并将响应转换为内部 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/stream/
```

## 测试文件索引

- `anthropic-request-stream-test.scm` — `anthropic-request-stream`：SSE 流式请求、thinking 块、错误响应与重试
