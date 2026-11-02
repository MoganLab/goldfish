# (liii pi ai api google-generative-ai core) 模块函数索引

`(liii pi ai api google-generative-ai core)` 负责 Gemini 协议中与具体端点无关的
核心转换：finishReason 与 usage 映射、工具调用 id 规范化、消息历史转换，
以及请求体组装。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai core))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-build-contents
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/core "gemini-build-contents"
```

## 函数分类索引

### 响应字段映射
- `gemini-map-finish-reason` - 将 Gemini 响应的 finishReason 映射为 pi agent 标准 stopReason
- `gemini-map-usage` - 将 Gemini 响应的 usageMetadata 映射为内部统一的 usage alist

### 工具调用 id
- `gemini-normalize-tool-call-id` - 规范化 Gemini 工具调用 id，保证只含字母、数字、下划线或连字符且长度不超过 64
- `gemini-generate-tool-call-id` - 依据工具名、时间戳与计数器生成规范化的 Gemini 工具调用 id

### 请求构造
- `gemini-build-contents` - 将内部消息历史转换为 Gemini API contents
- `gemini-build-request-body` - 组装发送给 Gemini API 的 JSON 请求体

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/core/
```

## 测试文件索引

- `gemini-map-finish-reason-test.scm` — `gemini-map-finish-reason`：finishReason 映射
- `gemini-map-usage-test.scm` — `gemini-map-usage`：usageMetadata 映射
- `gemini-normalize-tool-call-id-test.scm` — `gemini-normalize-tool-call-id`：id 字符规范化与截断
- `gemini-generate-tool-call-id-test.scm` — `gemini-generate-tool-call-id`：id 生成
- `gemini-build-contents-test.scm` — `gemini-build-contents`：消息历史转换与边界形态
- `gemini-build-request-body-test.scm` — `gemini-build-request-body`：请求体组装
- `gemini-parse-response-test.scm` — `gemini-parse-response`：Gemini 响应解析
- `gemini-is-retriable-error-p-test.scm` — `gemini-is-retriable-error?`：可重试错误判定
