# (liii pi ai api google-generative-ai) 模块函数索引

`(liii pi ai api google-generative-ai)` 是 Gemini API 的统一入口，
汇总并重新导出各子库的公开函数：

- `(liii pi ai api google-generative-ai core)` — 响应映射、工具 id 规范化、消息转换与请求体组装
- `(liii pi ai api google-generative-ai request)` — 端点构造与非流式请求
- `(liii pi ai api google-generative-ai stream)` — 流式请求
- `(liii pi ai api google-generative-ai stream-parse)` — SSE 事件解析与错误信息提取
- `(liii pi ai api google-generative-ai stream-result)` — 流式结果判定与结算
- `(liii pi ai api google-generative-ai stream-state)` — 流状态读写
- `(liii pi ai api google-generative-ai grounding)` — 搜索工具注入与接地元数据提取
- `(liii pi ai api google-generative-ai grounding-parse)` — 接地元数据解析
- `(liii pi ai api google-generative-ai thinking)` — 思考级别与 thinking 块处理

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai "gemini-request"
```

## 函数分类索引

### 响应字段映射
- `gemini-map-finish-reason` - 将 Gemini 响应的 finishReason 映射为 pi agent 标准 stopReason
- `gemini-map-usage` - 将 Gemini 响应的 usageMetadata 映射为内部统一的 usage alist

### 工具调用 id
- `gemini-normalize-tool-call-id` - 规范化 Gemini 工具调用 id，保证只含字母、数字、下划线或连字符且长度不超过 64
- `gemini-generate-tool-call-id` - 依据工具名、时间戳与计数器生成规范化的 Gemini 工具调用 id

### 请求构造与端点
- `gemini-build-contents` - 将内部消息历史转换为 Gemini API contents
- `gemini-build-request-body` - 组装发送给 Gemini API 的 JSON 请求体
- `gemini-build-tool-declarations` - 依据启用的工具名列表构造 Gemini API 的 functionDeclarations 工具声明
- `gemini-build-endpoint` - 构造发往 Gemini generateContent API 端的完整 URL
- `gemini-build-stream-endpoint` - 构造发往 Gemini streamGenerateContent API 端的完整 SSE URL

### 请求发起与解析
- `gemini-request` - 向 Gemini generateContent API 发起非流式请求，并转换为内部 assistant 消息
- `gemini-request-stream` - 向 Gemini generateContent 流式端点发起请求，并返回内部 assistant 消息
- `gemini-parse-response` - 将 Gemini generateContent JSON 响应解析为内部 assistant message
- `gemini-is-retriable-error?` - 判断 Gemini 协议的失败是否属于可重试错误
- `format-failed-stream-msg` - 根据失败响应生成流式请求的错误提示信息

### Google Search 接地
- `gemini-build-search-tools` - 构造包含 googleSearch 节的 Gemini tools 配置向量
- `gemini-inject-google-search` - 向 Gemini 请求体或已有的 tools 列表中注入 googleSearch 节
- `gemini-extract-grounding-details` - 从 Gemini 响应中提取搜索查询、搜索结果和引用信息
- `gemini-grounding-search-queries` - 从提取出的 groundingDetails 或原始响应中读取搜索关键词列表
- `gemini-grounding-search-results` - 从提取出的 groundingDetails 或原始响应中读取网页搜索结果列表
- `gemini-grounding-citations` - 从提取出的 groundingDetails 或原始响应中读取引用片段列表

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/
```

## 测试文件索引

- `gemini-map-finish-reason-test.scm` — `gemini-map-finish-reason`：finishReason 映射
- `gemini-map-usage-test.scm` — `gemini-map-usage`：usageMetadata 映射
- `gemini-normalize-tool-call-id-test.scm` — `gemini-normalize-tool-call-id`：工具 id 规范化
- `gemini-generate-tool-call-id-test.scm` — `gemini-generate-tool-call-id`：工具 id 生成
- `gemini-build-contents-test.scm` — `gemini-build-contents`：消息历史转换
- `gemini-build-request-body-test.scm` — `gemini-build-request-body`：请求体组装
- `gemini-build-tool-declarations-test.scm` — `gemini-build-tool-declarations`：工具声明构造
- `gemini-build-endpoint-test.scm` — `gemini-build-endpoint`：请求端点拼接
- `gemini-build-stream-endpoint-test.scm` — `gemini-build-stream-endpoint`：流式请求端点拼接
- `gemini-request-test.scm` — `gemini-request`：非流式请求集成测试
- `gemini-request-stream-test.scm` — `gemini-request-stream`：流式请求集成测试
- `gemini-parse-response-test.scm` — `gemini-parse-response`：响应解析
- `gemini-is-retriable-error-p-test.scm` — `gemini-is-retriable-error?`：可重试错误判定
- `format-failed-stream-msg-test.scm` — `format-failed-stream-msg`：失败提示格式化
- `gemini-build-search-tools-test.scm` — `gemini-build-search-tools`：搜索工具节构造
- `gemini-inject-google-search-test.scm` — `gemini-inject-google-search`：请求体工具注入
- `gemini-extract-grounding-details-test.scm` — `gemini-extract-grounding-details`：接地元数据提取
- `gemini-grounding-search-queries-test.scm` — `gemini-grounding-search-queries`：搜索查询读取
- `gemini-grounding-search-results-test.scm` — `gemini-grounding-search-results`：网页结果读取
- `gemini-grounding-citations-test.scm` — `gemini-grounding-citations`：引用片段读取
