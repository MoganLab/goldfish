# (liii pi ai api google-generative-ai usage) 模块函数索引

`(liii pi ai api google-generative-ai usage)` 负责 Gemini 响应元数据到内部标准
结构的映射：finishReason 映射为 stopReason，usageMetadata 映射为统一的 usage alist。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai usage))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-map-usage
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/usage "gemini-map-usage"
```

## 函数分类索引

### 响应字段映射
- `gemini-map-finish-reason` - 将 Gemini 响应的 finishReason 映射为 pi agent 标准 stopReason
- `gemini-retriable-finish-reason?` - 判断 finishReason 是否属于值得重发的可重试失败（仅 MALFORMED_RESPONSE）
- `gemini-map-usage` - 将 Gemini 响应的 usageMetadata 映射为内部统一的 usage alist

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/usage/
```

## 测试文件索引

- `gemini-map-finish-reason-test.scm` — `gemini-map-finish-reason`：finishReason 映射
- `gemini-retriable-finish-reason-p-test.scm` — `gemini-retriable-finish-reason?`：可重试 finishReason 判定
- `gemini-map-usage-test.scm` — `gemini-map-usage`：usageMetadata 映射
