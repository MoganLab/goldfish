# (liii pi ai api anthropic-messages input) 模块函数索引

`(liii pi ai api anthropic-messages input)` 负责 Anthropic 协议中「输入侧」
的归一化：把 toolCall 的 arguments 归一化成可安全序列化的 JSON 对象，
把响应里的 tool_use input 解析回内部 alist，并把内部 assistant block
转换成 Anthropic API 的 tool_use / text / thinking 格式。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages input))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-normalize-input
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/anthropic-messages/input "anthropic-normalize-input"
```

## 函数分类索引

### 参数归一化与解析
- `anthropic-normalize-input` - 归一化 toolCall 的 arguments/input，确保组装进 Anthropic API 请求时不会产生单元素 list 导致 json_write 抛出 value-error
- `anthropic-parse-tool-arguments` - 解析响应中的 tool_use input，统一转换为 Scheme 内部使用的 arguments alist

### 块格式转换
- `anthropic-assistant-blk->anthropic-blk` - 将 assistant message 中的 block 转换为 Anthropic API 所需的 tool_use / text / thinking 格式

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/anthropic-messages/input/
```

## 测试文件索引

- `anthropic-normalize-input-test.scm` — `anthropic-normalize-input`：空值、字符串、alist 与畸形 entry 的归一化
- `anthropic-parse-tool-arguments-test.scm` — `anthropic-parse-tool-arguments`：tool_use input 的反序列化与回退
- `anthropic-assistant-blk-to-anthropic-blk-test.scm` — `anthropic-assistant-blk->anthropic-blk`：text / toolCall / thinking 块转换
