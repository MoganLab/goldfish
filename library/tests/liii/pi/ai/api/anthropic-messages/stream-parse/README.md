# (liii pi ai api anthropic-messages stream-parse) 模块函数索引

`(liii pi ai api anthropic-messages stream-parse)` 负责 Anthropic SSE 流的解析：
把 SSE 事件对象分发到文本增量、工具参数增量与 thinking 增量的处理逻辑，
并在内容块结束时把文本块或 toolCall 块写入消息块列表。

流状态由 `(liii pi ai api anthropic-messages stream-state)` 提供，
思考块处理由 `(liii pi ai api anthropic-messages thinking)` 提供。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages stream-parse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc process-anthropic-sse
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/stream-parse "process-anthropic-sse"
```

## 函数分类索引

### 增量处理
- `append-anthropic-stream-text` - 将 Anthropic 流式文本增量追加到当前文本块，并派发 text_start / text_delta 事件
- `handle-anthropic-delta` - 处理 Anthropic content_block_delta 事件，按增量类型分发到文本、工具参数或 thinking 处理逻辑

### 内容块结束
- `stop-anthropic-current-block` - 结束 Anthropic 流式当前内容块，把工具调用或文本块写入消息块列表

### 事件分发
- `process-anthropic-sse` - 处理 Anthropic SSE 事件对象，按事件类型更新流状态并派发内部事件
- `make-anthropic-stream-decoder` - 创建 Anthropic SSE 流解码器，将文本、工具调用和 thinking 增量写入流状态

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/stream-parse/
```

## 测试文件索引

- `append-anthropic-stream-text-test.scm` — `append-anthropic-stream-text`：文本块合并与事件派发
- `handle-anthropic-delta-test.scm` — `handle-anthropic-delta`：文本、工具参数与 thinking 增量分发
- `stop-anthropic-current-block-test.scm` — `stop-anthropic-current-block`：工具块与文本块结束
- `process-anthropic-sse-test.scm` — `process-anthropic-sse`：SSE 事件类型分发与状态更新
- `make-anthropic-stream-decoder-test.scm` — `make-anthropic-stream-decoder`：SSE 解码器端到端
