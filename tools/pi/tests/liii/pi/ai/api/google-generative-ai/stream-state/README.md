# (liii pi ai api google-generative-ai stream-state) 模块函数索引

`(liii pi ai api google-generative-ai stream-state)` 提供 Gemini 流式过程中的
可变状态载体：保存已完成的消息块、当前累积文本、用量统计、工具调用列表，
以及当前活动块类型、思考文本与思考签名。

流状态的读取与更新均提供 record 与 vector 两种形态，vector 形态用于兼容旧调用。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai stream-state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-stream-state-blocks
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/stream-state "gemini-stream-state-blocks"
```

## 函数分类索引

### 状态构造与判定
- `make-gemini-stream-state` - 创建 Gemini 流状态记录
- `gemini-stream-state?` - 判断对象是否为 Gemini 流状态记录

### 消息块与文本
- `gemini-stream-state-blocks` - 读取 Gemini 流状态中的消息块列表
- `gemini-stream-state-blocks-set!` - 设置 Gemini 流状态中的消息块列表
- `gemini-stream-state-last-text` - 读取 Gemini 流状态中当前正在累积的文本
- `gemini-stream-state-last-text-set!` - 设置 Gemini 流状态中当前正在累积的文本

### 用量与停止原因
- `gemini-stream-state-usage` - 读取 Gemini 流状态中的用量统计
- `gemini-stream-state-usage-set!` - 设置 Gemini 流状态中的用量统计
- `gemini-stream-state-finish-reason` - 读取 Gemini 流状态中的停止原因
- `gemini-stream-state-finish-reason-set!` - 设置 Gemini 流状态中的停止原因
- `gemini-stream-state-tool-calls` - 读取 Gemini 流状态中已完成的工具调用列表
- `gemini-stream-state-tool-calls-set!` - 设置 Gemini 流状态中已完成的工具调用列表

### 当前活动块
- `gemini-stream-state-current-active-type` - 读取 Gemini 流状态中当前正在进行的块类型
- `gemini-stream-state-current-active-type-set!` - 设置 Gemini 流状态中当前正在进行的块类型
- `gemini-stream-state-thinking-text` - 读取 Gemini 流状态中当前正在累积的思考文本
- `gemini-stream-state-thinking-text-set!` - 设置 Gemini 流状态中当前正在累积的思考文本
- `gemini-stream-state-thinking-signature` - 读取 Gemini 流状态中当前思考块的签名
- `gemini-stream-state-thinking-signature-set!` - 设置 Gemini 流状态中当前思考块的签名

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/stream-state/
```

## 测试文件索引

- `make-gemini-stream-state-test.scm` — `make-gemini-stream-state`：流状态字段初始化
- `gemini-stream-state-p-test.scm` — `gemini-stream-state?`：流状态记录判定
- `gemini-stream-state-blocks-test.scm` / `gemini-stream-state-blocks-set-bang-test.scm` — 消息块列表读写
- `gemini-stream-state-last-text-test.scm` / `gemini-stream-state-last-text-set-bang-test.scm` — 当前文本读写
- `gemini-stream-state-usage-test.scm` / `gemini-stream-state-usage-set-bang-test.scm` — 用量统计读写
- `gemini-stream-state-tool-calls-test.scm` / `gemini-stream-state-tool-calls-set-bang-test.scm` — 工具调用列表读写
- `gemini-stream-state-finish-reason-test.scm` / `gemini-stream-state-finish-reason-set-bang-test.scm` — 停止原因读写
- `gemini-stream-state-current-active-type-test.scm` / `gemini-stream-state-current-active-type-set-bang-test.scm` — 活动块类型读写
- `gemini-stream-state-thinking-text-test.scm` / `gemini-stream-state-thinking-text-set-bang-test.scm` — 思考文本读写
- `gemini-stream-state-thinking-signature-test.scm` / `gemini-stream-state-thinking-signature-set-bang-test.scm` — 思考签名读写
