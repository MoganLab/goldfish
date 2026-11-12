# (liii pi agent queue) 模块函数索引

`(liii pi agent queue)` 提供 Agent 交互与调度所需的消息等待队列与待投递注入队列，包含空闲/繁忙状态机与多种消费粒度控制。

使用时导入：

```scheme
(import (liii pi agent queue))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-pi-message-queue
```

## 函数分类索引

### 调度消息队列 (<pi-message-queue>)
- `make-pi-message-queue` - 创建空闲状态的新消息调度队列
- `pi-message-queue?` - 判断是否为消息调度队列对象
- `pi-queue-empty?` - 判断调度队列是否为空
- `pi-queue-length` - 获取调度队列中的排队消息数量
- `pi-queue-idle?` - 判断调度队列是否处于空闲（idle）状态
- `pi-queue-busy?` - 判断调度队列是否处于繁忙（busy）状态
- `pi-queue-cancelling?` - 判断调度队列是否处于正在中止（cancelling）状态
- `pi-queue-items` - 获取调度队列中所有等待消费的消息列表
- `pi-queue-enqueue!` - 追加消息到调度队列尾部
- `pi-queue-dequeue-next!` - 在空闲状态下消费队首消息并转入繁忙态
- `pi-queue-finish-task!` - 通知当前任务完成并重置状态为空闲
- `pi-queue-set-busy!` - 标记调度队列为繁忙状态
- `pi-queue-set-cancelling!` - 标记调度队列为正在中止（cancelling）状态
- `pi-queue-restore-all!` - 撤回并排空调度队列中所有未消费消息

### 待注入消息队列 (<pi-pending-queue>)
- `make-pi-pending-queue` - 创建待注入消息队列对象（默认单条投递模式）
- `pi-pending-queue?` - 判断是否为待注入消息队列对象
- `pi-pending-mode` - 获取待注入队列当前的投递模式 ('one-at-a-time 或 'all)
- `pi-pending-mode-set!` - 设置待注入队列的投递模式
- `pi-pending-empty?` - 判断待注入队列是否为空
- `pi-pending-length` - 获取待注入队列中消息数量
- `pi-pending-items` - 获取待注入队列中全部排队消息列表
- `pi-pending-enqueue!` - 将消息追加到待注入队列末尾
- `pi-pending-peek` - 预览即将投递的消息（不从队列移除）
- `pi-pending-drain!` - 消费并移出当前符合投递模式的消息
- `pi-pending-clear!` - 清空并返回全部排队消息（用于撤回）
