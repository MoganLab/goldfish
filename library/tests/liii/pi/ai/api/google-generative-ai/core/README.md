# (liii pi ai api google-generative-ai core) 模块函数索引

`(liii pi ai api google-generative-ai core)` 负责 Gemini 协议中与具体端点无关的
消息历史转换与请求体组装。finishReason 与 usage 映射、工具调用 id 规范化分别位于
`usage`、`tool-call-id` 子模块，thought signature 回放位于 `thought-signature` 子模块。

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

### 请求构造
- `gemini-build-contents` - 将内部消息历史转换为 Gemini API contents
- `gemini-build-request-body` - 组装发送给 Gemini API 的 JSON 请求体

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/core/
```

## 测试文件索引

- `gemini-build-contents-test.scm` — `gemini-build-contents`：消息历史转换与边界形态
- `gemini-build-request-body-test.scm` — `gemini-build-request-body`：请求体组装
- `gemini-parse-response-test.scm` — `gemini-parse-response`：Gemini 响应解析
- `gemini-is-retriable-error-p-test.scm` — `gemini-is-retriable-error?`：可重试错误判定
