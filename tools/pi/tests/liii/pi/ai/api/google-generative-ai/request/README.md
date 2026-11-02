# (liii pi ai api google-generative-ai request) 模块函数索引

`(liii pi ai api google-generative-ai request)` 负责 Gemini generateContent API 的
非流式请求：构造请求端点、生成工具声明、解析响应、判定可重试错误，
并发起带重试的 HTTP 请求。

消息与 usage 的转换由 `(liii pi ai api google-generative-ai core)` 提供，
错误信息提取由 `(liii pi ai api google-generative-ai stream-parse)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai request))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/request "gemini-request"
```

## 函数分类索引

### 端点构造
- `gemini-build-endpoint` - 构造发往 Gemini generateContent API 端的完整 URL
- `gemini-build-stream-endpoint` - 构造发往 Gemini streamGenerateContent API 端的完整 SSE URL

### 工具声明
- `gemini-build-tool-declarations` - 依据启用的工具名列表构造 Gemini API 的 functionDeclarations 工具声明

### 响应解析
- `gemini-parse-response` - 将 Gemini generateContent JSON 响应解析为内部 assistant message
- `gemini-is-retriable-error?` - 判断 Gemini 协议的失败是否属于可重试错误
- `format-failed-stream-msg` - 根据失败响应生成流式请求的错误提示信息

### 请求发起
- `gemini-request` - 向 Gemini generateContent API 发起非流式请求，并转换为内部 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/request/
```

## 测试文件索引

- `gemini-build-endpoint-test.scm` — `gemini-build-endpoint`：请求端点拼接
- `gemini-build-stream-endpoint-test.scm` — `gemini-build-stream-endpoint`：流式请求端点拼接
- `gemini-build-tool-declarations-test.scm` — `gemini-build-tool-declarations`：工具声明构造
- `gemini-parse-response-test.scm` — `gemini-parse-response`：Gemini 响应解析
- `gemini-is-retriable-error-p-test.scm` — `gemini-is-retriable-error?`：可重试错误判定
- `format-failed-stream-msg-test.scm` — `format-failed-stream-msg`：失败提示格式化
- `gemini-request-test.scm` — `gemini-request`：非流式请求集成测试
