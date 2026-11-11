# (liii pi code all-in-one request) 模块函数分类索引

`(liii pi code all-in-one request)` 是 Agent 主循环与 all-in-one 调度器之间的请求适配器：按伪模型 ID 找链，预检后为健康节点解析真实凭据、协议与模型配置，再调用现有协议分发。

```scheme
(import (liii pi code all-in-one request))
```

## 函数分类索引

- `pi-all-in-one-request-stream` - 将元供应商请求转换为具体节点请求
- `pi-all-in-one-exhausted-message` - 构造全链枯竭的 assistant 错误消息
