# (liii pi ai api google-generative-ai stream-result) 模块函数索引

`(liii pi ai api google-generative-ai stream-result)` 负责 Gemini 流式请求的结算：
把流状态转换为内部 assistant 消息，并根据空闲超时、HTTP 状态码与网络错误
判定成功或失败。

流状态由 `(liii pi ai api google-generative-ai stream-state)` 提供，
错误信息提取由 `(liii pi ai api google-generative-ai stream-parse)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai stream-result))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-classify-stream-result
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/stream-result "gemini-classify-stream-result"
```

## 函数分类索引

### 流式结算
- `gemini-finalize-stream-success` - 结束 Gemini 流式请求并生成成功的 assistant 消息
- `gemini-classify-stream-result` - 根据流式请求的结束状态判定成功或失败，并生成对应的 assistant 消息

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/stream-result/
```

## 测试文件索引

- `gemini-finalize-stream-success-test.scm` — `gemini-finalize-stream-success`：成功结算与 stopReason 映射
- `gemini-classify-stream-result-test.scm` — `gemini-classify-stream-result`：空闲超时、HTTP 错误与网络错误分类
