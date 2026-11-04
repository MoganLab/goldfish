# (liii pi ai api openai-completions stream-state) 模块函数索引

`(liii pi ai api openai-completions stream-state)` 提供 OpenAI 流式过程中的
可变状态载体：保存已完成的消息块、当前累积文本、用量统计、停止原因、
错误信息，以及正在拼接的工具调用表。

流状态的读取与更新均提供 record 与 vector 两种形态，vector 形态用于兼容旧调用。

使用时导入：

```scheme
(import (liii pi ai api openai-completions stream-state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-stream-state-blocks
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/stream-state "openai-stream-state-blocks"
```

## 函数分类索引

### 状态构造与判定
- `make-openai-stream-state` - 创建 OpenAI 流状态记录
- `openai-stream-state?` - 判断对象是否为 OpenAI 流状态记录

### 消息块与文本
- `openai-stream-state-blocks` - 读取 OpenAI 流状态中的消息块列表
- `openai-stream-state-blocks-set!` - 设置 OpenAI 流状态中的消息块列表
- `openai-stream-state-last-text` - 读取 OpenAI 流状态中当前正在累积的文本
- `openai-stream-state-last-text-set!` - 设置 OpenAI 流状态中当前正在累积的文本
- `openai-stream-state-last-reasoning` - 读取 OpenAI 流状态中当前正在累积的思考内容
- `openai-stream-state-last-reasoning-set!` - 设置 OpenAI 流状态中当前正在累积的思考内容

### 用量与停止原因
- `openai-stream-state-usage` - 读取 OpenAI 流状态中的用量统计
- `openai-stream-state-usage-set!` - 设置 OpenAI 流状态中的用量统计
- `openai-stream-state-stop-reason` - 读取 OpenAI 流状态中的停止原因
- `openai-stream-state-stop-reason-set!` - 设置 OpenAI 流状态中的停止原因

### 错误与工具调用
- `openai-stream-state-error-message` - 读取 OpenAI 流状态中的错误信息
- `openai-stream-state-error-message-set!` - 设置 OpenAI 流状态中的错误信息
- `openai-stream-state-tools` - 读取 OpenAI 流状态中正在拼接的工具调用表
- `openai-stream-state-tools-set!` - 设置 OpenAI 流状态中正在拼接的工具调用表

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/stream-state/
```

## 测试文件索引

- `make-openai-stream-state-test.scm` — `make-openai-stream-state`：流状态字段初始化
- `openai-stream-state-p-test.scm` — `openai-stream-state?`：流状态记录判定
- `openai-stream-state-blocks-test.scm` / `openai-stream-state-blocks-set-bang-test.scm` — 消息块列表读写
- `openai-stream-state-last-text-test.scm` / `openai-stream-state-last-text-set-bang-test.scm` — 当前文本读写
- `openai-stream-state-last-reasoning-test.scm` / `openai-stream-state-last-reasoning-set-bang-test.scm` — 思考内容读写
- `openai-stream-state-usage-test.scm` / `openai-stream-state-usage-set-bang-test.scm` — 用量统计读写
- `openai-stream-state-stop-reason-test.scm` / `openai-stream-state-stop-reason-set-bang-test.scm` — 停止原因读写
- `openai-stream-state-error-message-test.scm` / `openai-stream-state-error-message-set-bang-test.scm` — 错误信息读写
- `openai-stream-state-tools-test.scm` / `openai-stream-state-tools-set-bang-test.scm` — 待拼接工具表读写
