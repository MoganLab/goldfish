# (liii pi ai api anthropic-messages request) 模块函数索引

`(liii pi ai api anthropic-messages request)` 负责 Anthropic Messages API 的
非流式请求：构造请求端点、把启用的工具名解析为 tools 声明，并发起带重试的
HTTP 请求，把响应转换为内部 assistant 消息。

请求体中消息与 usage 的转换由 `(liii pi ai api anthropic-messages core)` 提供，
思考级别注入由 `(liii pi ai api anthropic-messages thinking)` 提供。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages request))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/request "anthropic-request"
```

## 函数分类索引

### 端点与工具声明
- `anthropic-build-endpoint` - 构造发往 Anthropic Messages API 端的完整 URL
- `anthropic-build-tool-declarations-from-names` - 依据启用的工具名列表构造 Anthropic API 的 tools 声明

### 非流式请求
- `anthropic-request` - 向 Anthropic Messages API 发起非流式请求，并将响应转换为内部 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/request/
```

## 测试文件索引

- `anthropic-build-endpoint-test.scm` — `anthropic-build-endpoint`：官方与第三方兼容端点拼接
- `anthropic-build-tool-declarations-from-names-test.scm` — `anthropic-build-tool-declarations-from-names`：按工具名生成 tools 声明
- `anthropic-request-test.scm` — `anthropic-request`：普通请求、工具调用、截断、重试与兼容网关
