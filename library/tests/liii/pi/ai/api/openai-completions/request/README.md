# (liii pi ai api openai-completions request) 模块函数索引

`(liii pi ai api openai-completions request)` 负责 OpenAI Chat Completions API 的
非流式请求：构造请求端点与头信息、代理配置、工具声明，以及带退避重试与
API key 轮换的 HTTP 请求。

消息转换与请求体由 `(liii pi ai api openai-completions msg)` 提供，
响应解析与错误判定由 `(liii pi ai api openai-completions core)` 提供。

使用时导入：

```scheme
(import (liii pi ai api openai-completions request))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-request
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/request "openai-request"
```

## 函数分类索引

### 端点与请求头
- `openai-build-endpoint` - 构造发往 OpenAI Chat Completions 端点的完整 URL
- `openai-build-headers` - 构造发往 OpenAI Chat Completions API 的 HTTP 请求头
- `openai-build-proxy-alist` - 根据代理 URL 构造 HTTP/HTTPS 代理配置表

### 工具声明
- `openai-build-tool-declarations-from-names` - 根据内置工具名列表构建 OpenAI function calling 声明

### 非流式请求
- `openai-request` - OpenAI 兼容协议的非流式请求

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/request/
```

## 测试文件索引

- `openai-build-endpoint-test.scm` — `openai-build-endpoint`：端点路径拼接
- `openai-build-headers-test.scm` — `openai-build-headers`：请求头构造
- `openai-build-proxy-alist-test.scm` — `openai-build-proxy-alist`：代理配置构造
- `openai-build-tool-declarations-from-names-test.scm` — `openai-build-tool-declarations-from-names`：按名生成工具声明
- `openai-request-test.scm` — `openai-request`：非流式请求与额度耗尽测试
