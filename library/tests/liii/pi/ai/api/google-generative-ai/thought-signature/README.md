# (liii pi ai api google-generative-ai thought-signature) 模块函数索引

`(liii pi ai api google-generative-ai thought-signature)` 负责 Gemini 3 的
thought signature 回放：校验签名的 base64 合法性，并把 assistant 内容块
（text / thinking / toolCall）转换为 Gemini parts。

跨模型（provider 或 model 不同）时不回传签名，thinking 块被彻底过滤丢弃。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai thought-signature))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-valid-thought-signature?
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/thought-signature "gemini-assistant-block->part"
```

## 函数分类索引

### 签名校验
- `gemini-valid-thought-signature?` - 判断 thought signature 是否为合法的 base64（TYPE_BYTES）串

### 内容块转换
- `gemini-assistant-block->part` - 将 assistant 内容块转换为 Gemini part，同模型时回放 thought signature

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/thought-signature/
```

## 测试文件索引

- `gemini-valid-thought-signature-p-test.scm` — `gemini-valid-thought-signature?`：base64 签名合法性判定
- `gemini-assistant-block-to-part-test.scm` — `gemini-assistant-block->part`：内容块到 part 的转换
- `cross-model-switch-test.scm` — 跨模型切换场景下的思考签名校验与思维链阻断
