# (liii pi ai api openai-completions stream-parse) 模块函数索引

`(liii pi ai api openai-completions stream-parse)` 负责 OpenAI SSE 流的解析：
把 SSE chunk 分发到文本增量与工具调用参数增量，在流结束时组装完整的
toolCall 块，并从错误响应中提取错误文本。

流状态由 `(liii pi ai api openai-completions stream-state)` 提供。

使用时导入：

```scheme
(import (liii pi ai api openai-completions stream-parse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-openai-stream-decoder
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/stream-parse "make-openai-stream-decoder"
```

## 函数分类索引

### 增量处理
- `append-openai-stream-text` - 向 OpenAI 流状态追加文本增量，并派发 text_start / text_delta 事件
- `process-openai-tool-call-delta` - 处理 OpenAI 流式 tool_calls 增量，累积参数并在首个分块派发 toolcall_start

### 流式结束与分块
- `finish-openai-stream` - 完成 OpenAI 流式请求，将累积的工具调用转换为 toolCall 块写入 blocks
- `process-openai-chunk` - 处理单个 OpenAI Chat Completions chunk JSON 对象，更新用量、停止原因与文本/工具增量
- `make-openai-stream-decoder` - 创建 OpenAI Chat Completions SSE 流解码器，累积文本和工具调用增量

### 错误提取
- `extract-openai-stream-error-message` - 从 OpenAI 错误响应体中提取错误信息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/stream-parse/
```

## 测试文件索引

- `append-openai-stream-text-test.scm` — `append-openai-stream-text`：文本增量追加
- `process-openai-tool-call-delta-test.scm` — `process-openai-tool-call-delta`：工具参数增量拼接
- `finish-openai-stream-test.scm` — `finish-openai-stream`：工具调用结算
- `process-openai-chunk-test.scm` — `process-openai-chunk`：单个 chunk 分块处理
- `make-openai-stream-decoder-test.scm` — `make-openai-stream-decoder`：SSE 解码器
- `extract-openai-stream-error-message-test.scm` — `extract-openai-stream-error-message`：错误文本提取
