# (liii pi ai api google-generative-ai tool-call-id) 模块函数索引

`(liii pi ai api google-generative-ai tool-call-id)` 负责 Gemini 工具调用 id 的
规范化与生成：保证 id 只含字母、数字、下划线或连字符，且长度不超过 64。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai tool-call-id))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-normalize-tool-call-id
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/tool-call-id "gemini-normalize-tool-call-id"
```

## 函数分类索引

### 工具调用 id
- `gemini-normalize-tool-call-id` - 规范化 Gemini 工具调用 id，保证只含字母、数字、下划线或连字符且长度不超过 64
- `gemini-generate-tool-call-id` - 依据工具名、时间戳与计数器生成规范化的 Gemini 工具调用 id

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/tool-call-id/
```

## 测试文件索引

- `gemini-normalize-tool-call-id-test.scm` — `gemini-normalize-tool-call-id`：id 字符规范化与截断
- `gemini-generate-tool-call-id-test.scm` — `gemini-generate-tool-call-id`：id 生成
