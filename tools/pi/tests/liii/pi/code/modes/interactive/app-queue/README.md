# (liii pi code modes interactive app-queue) 模块函数索引

`(liii pi code modes interactive app-queue)` 负责交互式会话的队列注入与撤回：
steering（busy 时回车）与 follow-up（busy 时 alt+enter）消息的通道写入与
显示列表同步，以及 ESC 撤回时的待处理消息回收（对齐上游 pi 的
`queueSteering` / `queueFollowUp` / `restoreQueues`）。

使用时导入：

```scheme
(import (liii pi code modes interactive app-queue))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 注入消息
- `pi-app-queue-steering!` - steering 注入（busy 时回车）：写入通道 + 追加显示列表
- `pi-app-queue-follow-up!` - follow-up 注入（busy 时 alt+enter）：写入通道 + 追加显示列表

### 消费同步与撤回
- `pi-app-remove-pending-message!` - worker 消费注入消息后同步显示列表
- `pi-app-restore-queues!` - ESC 撤回全部排队消息并清空显示列表
- `pi-app-handle-escape!` - 处理 ESC 键：清除选区或发出中止信号、标记 cancelling 并撤回排队消息
