# (liii pi ai api openai-completions core) 模块函数索引

`(liii pi ai api openai-completions core)` 负责 OpenAI Chat Completions 协议中
与具体端点无关的核心转换：finish_reason 映射、usage 映射、响应解析，
以及可重试错误判定。

使用时导入：

```scheme
(import (liii pi ai api openai-completions core))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-parse-response
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/core "openai-parse-response"
```

## 函数分类索引

### 响应字段映射
- `openai-map-stop-reason` - 将 Kimi/OpenAI 响应 finish_reason 映射到 pi agent 标准 stopReason
- `openai-map-usage` - 将 Kimi/OpenAI 响应 usage 对象映射为标准 usage alist

### 响应解析与重试判定
- `openai-parse-response` - 将 OpenAI Chat Completions JSON 响应解析为内部 assistant message
- `openai-is-retriable-error?` - 判断 OpenAI 兼容协议的失败是否属于可重试错误

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/core/
```

## 测试文件索引

- `openai-map-stop-reason-test.scm` — `openai-map-stop-reason`：stop_reason 映射
- `openai-map-usage-test.scm` — `openai-map-usage`：usage 字段映射与缺省补零
- `openai-parse-response-test.scm` — `openai-parse-response`：文本、工具调用与错误响应解析
- `openai-is-retriable-error-p-test.scm` — `openai-is-retriable-error?`：可重试状态码与额度耗尽判定
