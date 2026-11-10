# (liii pi ai transcript) 模块函数索引

`(liii pi ai transcript)` 提供请求前转录规范化（对应上游 pi-ai 的 `api/transform-messages.ts`）：在构建 provider 请求体前统一规范化消息列表，跳过 error/aborted 的 assistant 消息、过滤异构模型的思维链并剥离工具思考签名、为孤儿 toolCall 补合成 toolResult、按需归一化 toolCallId，保证请求前缀跨轮稳定。仅作用于请求构建，不改会话持久化。

跨模型判定与内容块净化由子库 `(liii pi ai transcript cross-model)` 提供：

```scheme
(import (liii pi ai transcript))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-normalize-transcript
```

## 函数分类索引

### 转录规范化
- `pi-normalize-transcript` - 规范化消息列表（error/aborted 过滤、跨模型思维链过滤与签名剥离、孤儿 toolCall 补结果、toolCallId 归一化）
- `pi-transcript-normalize-tool-call-id?` - 当前请求是否需要归一化 toolCall id（仅 Gemini 3+）

### 跨模型判定（子库 `(liii pi ai transcript cross-model)`）
- `pi-transcript-same-model?` - 判定历史助手消息是否与当前请求目标属于同一模型
- `pi-transcript-normalize-block` - 内容块净化：跨模型时丢弃思维链并剥离私有签名
