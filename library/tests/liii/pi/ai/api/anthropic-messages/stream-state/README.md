# (liii pi ai api anthropic-messages stream-state) 模块函数索引

`(liii pi ai api anthropic-messages stream-state)` 提供 Anthropic 流式过程中的
可变状态载体：保存已完成的消息块、当前累积文本、用量统计、工具调用列表，
以及当前正在进行的工具块与 thinking 块状态。

流状态的读取与更新均提供 record 与 vector 两种形态，vector 形态用于兼容旧调用。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages stream-state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-stream-state-blocks
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/stream-state "anthropic-stream-state-blocks"
```

## 函数分类索引

### 状态构造与判定
- `make-anthropic-stream-state` - 创建 Anthropic 流状态记录
- `anthropic-stream-state?` - 判断对象是否为 Anthropic 流状态记录

### 消息块与文本
- `anthropic-stream-state-blocks` - 读取 Anthropic 流状态中的消息块列表
- `anthropic-stream-state-blocks-set!` - 设置 Anthropic 流状态中的消息块列表
- `anthropic-stream-state-last-text` - 读取 Anthropic 流状态中当前正在累积的文本
- `anthropic-stream-state-last-text-set!` - 设置 Anthropic 流状态中当前正在累积的文本

### 用量与停止原因
- `anthropic-stream-state-usage` - 读取 Anthropic 流状态中的用量统计
- `anthropic-stream-state-usage-set!` - 设置 Anthropic 流状态中的用量统计
- `anthropic-stream-state-stop-reason` - 读取 Anthropic 流状态中的停止原因
- `anthropic-stream-state-stop-reason-set!` - 设置 Anthropic 流状态中的停止原因

### 工具调用与当前块
- `anthropic-stream-state-tool-calls` - 读取 Anthropic 流状态中已完成的工具调用列表
- `anthropic-stream-state-tool-calls-set!` - 设置 Anthropic 流状态中已完成的工具调用列表
- `anthropic-stream-state-current-tool` - 读取 Anthropic 流状态中当前正在进行的工具块状态
- `anthropic-stream-state-current-tool-set!` - 设置 Anthropic 流状态中当前正在进行的工具块状态
- `anthropic-stream-state-current-thinking` - 读取 Anthropic 流状态中当前正在进行的 thinking 块状态
- `anthropic-stream-state-current-thinking-set!` - 设置 Anthropic 流状态中当前正在进行的 thinking 块状态

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/stream-state/
```

## 测试文件索引

- `make-anthropic-stream-state-test.scm` — `make-anthropic-stream-state`：流状态字段初始化
- `anthropic-stream-state-p-test.scm` — `anthropic-stream-state?`：流状态记录判定
- `anthropic-stream-state-blocks-test.scm` / `anthropic-stream-state-blocks-set-bang-test.scm` — 消息块列表读写
- `anthropic-stream-state-last-text-test.scm` / `anthropic-stream-state-last-text-set-bang-test.scm` — 当前文本读写
- `anthropic-stream-state-usage-test.scm` / `anthropic-stream-state-usage-set-bang-test.scm` — 用量统计读写
- `anthropic-stream-state-stop-reason-test.scm` / `anthropic-stream-state-stop-reason-set-bang-test.scm` — 停止原因读写
- `anthropic-stream-state-tool-calls-test.scm` / `anthropic-stream-state-tool-calls-set-bang-test.scm` — 工具调用列表读写
- `anthropic-stream-state-current-tool-test.scm` / `anthropic-stream-state-current-tool-set-bang-test.scm` — 当前工具块状态读写
- `anthropic-stream-state-current-thinking-test.scm` / `anthropic-stream-state-current-thinking-set-bang-test.scm` — 当前 thinking 块状态读写
