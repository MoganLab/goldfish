# (liii pi agent tools) 模块函数索引

`(liii pi agent tools)` 提供 Agent 工具调用的执行器、状态流转、扩展拦截以及消息与结果块的提取转换功能。

使用时导入：

```scheme
(import (liii pi agent tools))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-execute-tool-calls
```

## 函数分类索引

### 键值与内容提取
- `pi-agent-assoc-val` - 兼容符号键与字符串键的 alist 安全取值
- `pi-first-block-from-content` - 从消息 content 中安全提取首个数据块
- `pi-extract-tool-result-text` - 从工具执行结果中提取纯文本输出
- `extract-assistant-text` - 从 Assistant 消息中提取并合并所有文本块
- `pi-filter-tool-calls` - 过滤提取 content 中的所有 toolCall 块
- `pi-extract-tool-result-blocks` - 从会话末尾提取指定数量的工具结果消息块

### 消息构造
- `pi-make-tool-result-msg` - 构造单条 toolResult 消息对象

### 工具调用执行
- `pi-execute-one-tool-call` - 执行单个工具调用并返回结果消息
- `pi-execute-one-tool-call-with-status` - 执行单个工具调用并返回结果与终止标志
- `pi-execute-tool-calls` - 批量执行工具调用列表并返回结果消息列表
- `pi-execute-tool-calls-with-status` - 批量执行工具调用并返回状态与结果
- `pi-fail-tool-calls-length` - 为超长截断未执行的工具调用批量构造跳过失败结果
