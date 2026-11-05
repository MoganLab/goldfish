# (liii pi ai abort) 模块函数索引

`(liii pi ai abort)` 提供 AI 请求与 Agent 工作流共用的会话级中止信号管理，支持绑定通道、检查中止状态和复位。

使用时导入：

```scheme
(import (liii pi ai abort))
```

## 函数索引

- `pi-abort-arm-chan!` - 绑定中止信号通道并排空残留信号
- `pi-aborted?` - 非阻塞检查是否已收到中止信号
- `pi-abort-reset!` - 解除通道绑定并复位中止状态
