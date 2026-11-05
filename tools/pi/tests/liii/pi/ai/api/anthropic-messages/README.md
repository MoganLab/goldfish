# (liii pi ai api anthropic-messages) 模块函数索引

`(liii pi ai api anthropic-messages)` 是 Anthropic Messages API 的统一入口，
汇总并重新导出各子库的公开函数：

- `(liii pi ai api anthropic-messages core)` — 响应字段映射、请求体组装与响应解析
- `(liii pi ai api anthropic-messages cache)` — prompt caching 断点构造（system 块 / 末 tool / 末条消息）
- `(liii pi ai api anthropic-messages input)` — 工具参数归一化与解析
- `(liii pi ai api anthropic-messages request)` — 端点构造与非流式请求
- `(liii pi ai api anthropic-messages stream)` — 流式请求
- `(liii pi ai api anthropic-messages stream-parse)` — SSE 事件解析
- `(liii pi ai api anthropic-messages stream-state)` — 流状态读写
- `(liii pi ai api anthropic-messages thinking)` — 思考级别与 thinking 块处理

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages "anthropic-request"
```

## 函数分类索引

### 响应字段映射
- `anthropic-map-stop-reason` - 将 Anthropic 响应的 stop_reason 映射到 pi agent 标准 stopReason
- `anthropic-map-usage` - 将 Anthropic 响应的 usage 统计映射为内部统一的 usage alist

### 请求构造
- `anthropic-build-tool-declarations` - 构造符合 Anthropic 规格的 input_schema 工具声明
- `anthropic-build-tool-declarations-from-names` - 依据启用的工具名列表构造 Anthropic API 的 tools 声明
- `anthropic-build-messages` - 将内部会话消息历史转换为 Anthropic Messages API 消息
- `anthropic-build-request-body` - 组装发送给 Anthropic API 的 JSON 请求体
- `anthropic-build-endpoint` - 构造发往 Anthropic Messages API 端的完整 URL

### 响应解析
- `anthropic-parse-response` - 解析 Anthropic API 返回的 JSON 响应，转换为内部 assistant 消息
- `anthropic-is-retriable-error?` - 判断 Anthropic 协议的失败是否属于可重试错误

### 参数归一化
- `anthropic-normalize-input` - 归一化 toolCall 的 arguments/input，避免产生单元素 list
- `anthropic-parse-tool-arguments` - 解析响应中的 tool_use input，转换为内部 arguments alist

### 请求发起
- `anthropic-request` - 向 Anthropic Messages API 发起非流式请求，并转换为内部 assistant 消息
- `anthropic-request-stream` - 向 Anthropic Messages API 发起流式请求，并转换为内部 assistant 消息

### 流式解析
- `append-anthropic-stream-text` - 将 Anthropic 流式文本增量追加到当前文本块
- `handle-anthropic-delta` - 处理 Anthropic content_block_delta 事件并分发增量
- `stop-anthropic-current-block` - 结束 Anthropic 流式当前内容块
- `process-anthropic-sse` - 处理 Anthropic SSE 事件对象并更新流状态
- `make-anthropic-stream-decoder` - 创建 Anthropic SSE 流解码器

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/
```

## 测试文件索引

- `anthropic-map-stop-reason-test.scm` — `anthropic-map-stop-reason`：stop_reason 映射
- `anthropic-map-usage-test.scm` — `anthropic-map-usage`：usage 字段映射
- `anthropic-build-tool-declarations-test.scm` — `anthropic-build-tool-declarations`：工具声明构造
- `anthropic-build-tool-declarations-from-names-test.scm` — `anthropic-build-tool-declarations-from-names`：按工具名生成 tools 声明
- `anthropic-build-messages-test.scm` — `anthropic-build-messages`：消息历史转换与边界形态
- `anthropic-build-request-body-test.scm` — `anthropic-build-request-body`：请求体组装
- `anthropic-build-endpoint-test.scm` — `anthropic-build-endpoint`：端点拼接
- `anthropic-parse-response-test.scm` — `anthropic-parse-response`：响应解析
- `anthropic-is-retriable-error-p-test.scm` — `anthropic-is-retriable-error?`：可重试错误判定
- `anthropic-normalize-input-test.scm` — `anthropic-normalize-input`：参数归一化
- `anthropic-parse-tool-arguments-test.scm` — `anthropic-parse-tool-arguments`：工具参数反序列化
- `anthropic-request-test.scm` — `anthropic-request`：非流式请求集成测试
- `anthropic-request-stream-test.scm` — `anthropic-request-stream`：流式请求集成测试
- `append-anthropic-stream-text-test.scm` — `append-anthropic-stream-text`：文本块合并
- `handle-anthropic-delta-test.scm` — `handle-anthropic-delta`：增量分发
- `stop-anthropic-current-block-test.scm` — `stop-anthropic-current-block`：内容块结束
- `process-anthropic-sse-test.scm` — `process-anthropic-sse`：SSE 事件分发
- `make-anthropic-stream-decoder-test.scm` — `make-anthropic-stream-decoder`：SSE 解码器
