# (liii pi ai api google-generative-ai stream-parse) 模块函数索引

`(liii pi ai api google-generative-ai stream-parse)` 负责 Gemini SSE 流的解析：
把 SSE 数据块分发到文本、thinking 与函数调用的处理逻辑，并在流失败时
格式化错误信息。

流状态由 `(liii pi ai api google-generative-ai stream-state)` 提供，
思考块处理由 `(liii pi ai api google-generative-ai thinking)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai stream-parse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc process-stream-part
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/stream-parse "process-stream-part"
```

## 函数分类索引

### 文本块
- `emit-text-start-and-delta` - 派发 text_start 与 text_delta 事件，标记文本块开始并推送增量
- `append-text-block` - 把文本增量追加到 Gemini 流状态的消息块列表，并派发对应事件

### 内容块处理
- `process-stream-part` - 处理 Gemini 流式响应中的单个 part，并更新文本、thinking 或工具调用状态
- `process-stream-function-call` - 处理 Gemini 流式响应中的 functionCall，写入工具调用块并派发工具事件

### SSE 事件分发
- `process-sse-chunk-data` - 处理 Gemini 流式响应的一个 SSE 数据块，更新用量、停止原因与内容块
- `make-stream-decoder` - 创建 Gemini SSE 流解码器，把 SSE 数据块解析为内部消息块与事件

### 错误信息
- `extract-stream-error-message` - 从 Gemini 错误响应体中提取错误信息
- `format-failed-stream-msg` - 根据失败响应生成流式请求的错误提示信息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/stream-parse/
```

## 测试文件索引

- `emit-text-start-and-delta-test.scm` — `emit-text-start-and-delta`：文本事件派发
- `append-text-block-test.scm` — `append-text-block`：文本块追加与合并
- `process-stream-part-test.scm` — `process-stream-part`：单个 part 分发
- `process-stream-part-thinking-test.scm` — `process-stream-part`：thinking 与文本块切换
- `process-stream-function-call-test.scm` — `process-stream-function-call`：函数调用块写入
- `process-sse-chunk-data-test.scm` — `process-sse-chunk-data`：SSE 数据块处理
- `make-stream-decoder-test.scm` — `make-stream-decoder`：SSE 解码器
- `extract-stream-error-message-test.scm` — `extract-stream-error-message`：错误信息提取
- `format-failed-stream-msg-test.scm` — `format-failed-stream-msg`：失败提示格式化
