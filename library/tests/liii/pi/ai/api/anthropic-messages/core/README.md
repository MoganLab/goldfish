# (liii pi ai api anthropic-messages core) 模块函数索引

`(liii pi ai api anthropic-messages core)` 负责 Anthropic 协议中与具体端点无关的
核心转换：stop_reason 与 usage 映射、工具声明构造、消息历史转换、请求体组装，
以及响应解析与可重试错误判定。

参数的归一化与解析由 `(liii pi ai api anthropic-messages input)` 提供。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages core))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-parse-response
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/core "anthropic-parse-response"
```

## 函数分类索引

### 响应字段映射
- `anthropic-map-stop-reason` - 将 Anthropic 响应的 stop_reason 映射为 pi agent 标准 stopReason
- `anthropic-map-usage` - 将 Anthropic 响应的 usage 统计映射为内部统一的 usage alist

### 请求构造
- `anthropic-build-tool-declarations` - 构造符合 Anthropic 规格的 input_schema 工具声明
- `anthropic-build-messages` - 将内部会话消息历史转换为 Anthropic Messages API 消息
- `anthropic-build-request-body` - 组装发送给 Anthropic API 的 JSON 请求体

### 响应解析
- `anthropic-parse-response` - 解析 Anthropic API 返回的 JSON 响应，转换为内部 assistant 消息
- `anthropic-is-retriable-error?` - 判断 Anthropic 协议的失败是否属于可重试错误

### 参数归一化
- `anthropic-normalize-input` - 归一化 toolCall 的 arguments/input，避免产生单元素 list
- `anthropic-parse-tool-arguments` - 解析响应中的 tool_use input，转换为内部 arguments alist

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/core/
```

## 测试文件索引

- `anthropic-map-stop-reason-test.scm` — `anthropic-map-stop-reason`：stop_reason 映射
- `anthropic-map-usage-test.scm` — `anthropic-map-usage`：usage 字段映射与缺省补零
- `anthropic-build-tool-declarations-test.scm` — `anthropic-build-tool-declarations`：工具声明构造
- `anthropic-build-messages-test.scm` — `anthropic-build-messages`：消息历史转换与边界形态
- `anthropic-build-request-body-test.scm` — `anthropic-build-request-body`：请求体组装
- `anthropic-parse-response-test.scm` — `anthropic-parse-response`：文本、工具调用与错误响应解析
- `anthropic-is-retriable-error-p-test.scm` — `anthropic-is-retriable-error?`：可重试状态码与额度耗尽判定
- `anthropic-normalize-input-test.scm` — `anthropic-normalize-input`：参数归一化
- `anthropic-parse-tool-arguments-test.scm` — `anthropic-parse-tool-arguments`：工具参数反序列化
