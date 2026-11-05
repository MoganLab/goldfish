# (liii pi ai api openai-completions stream) 模块函数索引

`(liii pi ai api openai-completions stream)` 负责 OpenAI Chat Completions API 的
流式请求：构造请求端点、发起 SSE 流式请求，并在流式生命周期中处理自动重试、
API key 轮换、空闲超时与用户中止。

端点构造由 `(liii pi ai api openai-completions request)` 提供，
SSE 解析与状态维护由 `(liii pi ai api openai-completions stream-parse)`
和 `(liii pi ai api openai-completions stream-state)` 提供。

使用时导入：

```scheme
(import (liii pi ai api openai-completions stream))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-request-stream
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/stream "openai-request-stream"
```

## 函数分类索引

### 流式请求
- `openai-request-stream` - 向 OpenAI Chat Completions API 发起流式请求，并返回内部 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/stream/
```

## 测试文件索引

- `openai-request-stream-test.scm` — `openai-request-stream`：SSE 流式请求、工具调用与用量更新
- `openai-request-stream-abort-test.scm` — `openai-request-stream`：用户中止（abort/ESC）端到端集成测试
