# (liii pi ai api google-generative-ai thinking) 模块函数索引

`(liii pi ai api google-generative-ai thinking)` 负责 Gemini 协议的思考（thinking）支持：
把内部思考级别映射为 Gemini 的 thinkingLevel、向请求体注入 thinkingConfig，
识别响应中的思考 part，并在流式过程中维护 thinking 块的开始与结束。

流状态由 `(liii pi api google-generative-ai stream-state)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai thinking))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-inject-thinking-config
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/thinking "gemini-inject-thinking-config"
```

## 函数分类索引

### 思考级别与请求体
- `gemini-thinking-level->google-level` - 将内部思考级别映射为 Gemini API 的 thinkingLevel
- `gemini-build-thinking-config` - 按思考级别构造 Gemini 请求的 thinkingConfig 片段
- `gemini-inject-thinking-config` - 按思考级别向 Gemini 请求体注入 generationConfig.thinkingConfig

### 响应 part 解析
- `gemini-is-thinking-part?` - 判断 Gemini part 是否为思考内容
- `gemini-part-text` - 读取 Gemini part 中的文本内容
- `gemini-part-thought-signature` - 读取 Gemini part 中的思考签名

### 流式 thinking 块
- `gemini-stream-state-active-type` - 读取 Gemini 流状态中当前正在进行的块类型
- `gemini-stream-stop-active-block!` - 结束 Gemini 流中当前活动的文本或 thinking 块
- `gemini-append-thinking-block!` - 向 Gemini 流状态追加 thinking 文本和可选签名

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/thinking/
```

## 测试文件索引

- `gemini-thinking-level-to-google-level-test.scm` — `gemini-thinking-level->google-level`：思考级别映射
- `gemini-build-thinking-config-test.scm` — `gemini-build-thinking-config`：thinkingConfig 构造
- `gemini-inject-thinking-config-test.scm` — `gemini-inject-thinking-config`：请求体注入
- `gemini-is-thinking-part-p-test.scm` — `gemini-is-thinking-part?`：思考 part 判定
- `gemini-part-text-test.scm` — `gemini-part-text`：part 文本读取
- `gemini-part-thought-signature-test.scm` — `gemini-part-thought-signature`：part 签名读取
- `gemini-stream-state-active-type-test.scm` — `gemini-stream-state-active-type`：活动块类型读取
- `gemini-stream-stop-active-block-bang-test.scm` — `gemini-stream-stop-active-block!`：活动块结束
- `gemini-append-thinking-block-bang-test.scm` — `gemini-append-thinking-block!`：thinking 块追加
