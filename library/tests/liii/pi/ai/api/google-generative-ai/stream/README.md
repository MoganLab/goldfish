# (liii pi ai api google-generative-ai stream) 模块函数索引

`(liii pi ai api google-generative-ai stream)` 负责 Gemini 流式请求发起与流程管理：
拼装请求体、发起 SSE 流式请求，并在流式生命周期中处理重试、API key 轮换、
空闲超时与用户中止。

端点构造由 `(liii pi ai api google-generative-ai request)` 提供，
SSE 解析由 `(liii pi ai api google-generative-ai stream-parse)` 提供，
结果结算由 `(liii pi ai api google-generative-ai stream-result)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai stream))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-request-stream
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/stream "gemini-request-stream"
```

## 函数分类索引

### 流式请求
- `gemini-request-stream` - 向 Gemini generateContent 流式端点发起请求，并返回内部 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/stream/
```

## 测试文件索引

- `gemini-request-stream-test.scm` — `gemini-request-stream`：SSE 流式请求、thinking 块、错误重试与 key 轮换
