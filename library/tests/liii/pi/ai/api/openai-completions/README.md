# (liii pi ai api openai-completions) 模块函数索引

`(liii pi ai api openai-completions)` 是 OpenAI Chat Completions 协议的统一入口，
汇总并重新导出各子库的公开函数：

- `(liii pi ai api openai-completions core)` — 响应字段映射、响应解析与重试判定
- `(liii pi ai api openai-completions msg)` — 工具声明转换、消息历史转换与请求体组装
- `(liii pi ai api openai-completions request)` — 端点构造、请求头与非流式请求
- `(liii pi ai api openai-completions stream)` — 流式请求
- `(liii pi ai api openai-completions stream-parse)` — SSE 事件解析与增量累积
- `(liii pi ai api openai-completions stream-state)` — 流状态读写

使用时导入：

```scheme
(import (liii pi ai api openai-completions))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions "openai-request"
```

## 函数分类索引

### 响应字段映射
- `openai-map-stop-reason` - 将 Kimi/OpenAI 响应 finish_reason 映射到 pi agent 标准 stopReason
- `openai-map-usage` - 将 Kimi/OpenAI 响应 usage 对象映射为标准 usage alist

### 请求构造与端点
- `openai-build-tool-declarations` - 将内部 tool-definition records 转换为 OpenAI function calling 格式
- `openai-build-tool-declarations-from-names` - 根据内置工具名列表构建 OpenAI function calling 声明
- `openai-build-messages` - 将内部消息历史转换为 OpenAI Chat Completions messages
- `openai-build-request-body` - 组装 OpenAI Chat Completions 请求体 alist
- `openai-build-endpoint` - 构造发往 OpenAI Chat Completions 端点的完整 URL
- `openai-build-headers` - 构造发往 OpenAI Chat Completions API 的 HTTP 请求头
- `openai-build-proxy-alist` - 根据代理 URL 构造 HTTP/HTTPS 代理配置表

### 响应解析与重试判定
- `openai-parse-response` - 将 OpenAI Chat Completions JSON 响应解析为内部 assistant message
- `openai-is-retriable-error?` - 判断 OpenAI 兼容协议的失败是否属于可重试错误

### 请求发起
- `openai-request` - OpenAI 兼容协议的非流式请求
- `openai-request-stream` - 向 OpenAI Chat Completions API 发起流式请求

### 流式解析
- `append-openai-stream-text` - 向 OpenAI 流状态追加文本增量，并派发 text_start / text_delta 事件
- `process-openai-tool-call-delta` - 处理 OpenAI 流式 tool_calls 增量，累积参数
- `finish-openai-stream` - 完成 OpenAI 流式请求，将累积的工具调用转换为 toolCall 块
- `process-openai-chunk` - 处理单个 OpenAI Chat Completions chunk JSON 对象
- `make-openai-stream-decoder` - 创建 OpenAI Chat Completions SSE 流解码器
- `extract-openai-stream-error-message` - 从 OpenAI 错误响应体中提取错误信息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/
```

## 测试文件索引

- `openai-map-stop-reason-test.scm` — `openai-map-stop-reason`：stop_reason 映射
- `openai-map-usage-test.scm` — `openai-map-usage`：usage 字段映射
- `openai-parse-response-test.scm` — `openai-parse-response`：响应解析
- `openai-is-retriable-error-p-test.scm` — `openai-is-retriable-error?`：可重试错误判定
- `openai-build-tool-declarations-test.scm` — `openai-build-tool-declarations`：工具声明格式转换
- `openai-build-tool-declarations-from-names-test.scm` — `openai-build-tool-declarations-from-names`：按名生成工具声明
- `openai-build-messages-test.scm` — `openai-build-messages`：消息历史转换
- `openai-build-request-body-test.scm` — `openai-build-request-body`：请求体组装
- `openai-build-endpoint-test.scm` — `openai-build-endpoint`：端点拼接
- `openai-build-headers-test.scm` — `openai-build-headers`：请求头构造
- `openai-build-proxy-alist-test.scm` — `openai-build-proxy-alist`：代理配置构造
- `openai-request-test.scm` — `openai-request`：非流式请求集成测试
- `openai-request-stream-test.scm` — `openai-request-stream`：流式请求集成测试
- `append-openai-stream-text-test.scm` — `append-openai-stream-text`：文本增量追加
- `process-openai-tool-call-delta-test.scm` — `process-openai-tool-call-delta`：工具参数增量拼接
- `finish-openai-stream-test.scm` — `finish-openai-stream`：工具调用结算
- `process-openai-chunk-test.scm` — `process-openai-chunk`：单个 chunk 分块处理
- `make-openai-stream-decoder-test.scm` — `make-openai-stream-decoder`：SSE 解码器
- `extract-openai-stream-error-message-test.scm` — `extract-openai-stream-error-message`：错误文本提取
