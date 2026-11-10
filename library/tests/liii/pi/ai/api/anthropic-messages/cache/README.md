# (liii pi ai api anthropic-messages cache) 模块函数索引

`(liii pi ai api anthropic-messages cache)` 提供 Anthropic prompt caching 断点构造：在 system 块、最后一个工具声明、最后一条 user 消息的末尾 content 块三处挂 `cache_control: {"type": "ephemeral"}` 断点，断点前的稳定前缀可被供应商复用，提升缓存命中率。

使用时导入：

```scheme
(import (liii pi ai api anthropic-messages cache))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc anthropic-system-blocks
```

## 函数分类索引

### 缓存断点
- `anthropic-system-blocks` - system 提示词转为挂断点的 content block 数组
- `anthropic-tool-decls-add-cache-control` - 最后一个工具声明挂 cache_control 断点
- `anthropic-messages-add-cache-control` - 最后一条 user 消息末尾 content 块挂断点
