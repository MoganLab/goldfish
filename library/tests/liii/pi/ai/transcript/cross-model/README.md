# (liii pi ai transcript cross-model) 模块函数索引

`(liii pi ai transcript cross-model)` 提供协议无关的跨模型判定与内容块净化（对应上游 pi-ai 的 `api/transform-messages.ts`）：判定历史助手消息是否与当前请求目标同源，并在异构模型下丢弃思维链、剥离工具思考签名与文本块私有签名。仅作用于请求构建链路，不改会话持久化。

使用时导入：

```scheme
(import (liii pi ai transcript cross-model))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-transcript-same-model?
```

## 函数分类索引

### 跨模型判定
- `pi-transcript-same-model?` - 判定历史助手消息是否与当前请求目标属于同一模型

### 内容块净化
- `pi-transcript-normalize-block` - 跨模型时丢弃思维链并剥离私有签名
