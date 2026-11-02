# (liii pi agent loop-queue) 模块函数索引

`(liii pi agent loop-queue)` 提供 Agent 主循环中的排队消息轮询与注入机制，支持用户方向引导（steering messages）与任务跟进消息（follow-up messages）在各轮次及自然停止前的检测与派发。

使用时导入：

```scheme
(import (liii pi agent loop-queue))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %poll-steering
```

## 函数分类索引

### 消息轮询
- `%get-poll-fn` - 从 Agent 配置中提取指定的队列轮询回调过程
- `%poll-config-messages` - 调用指定的轮询回调并安全获取待处理消息
- `%poll-steering` - 轮询配置中的 steering 引导消息队列

### 消息注入与流转
- `%inject-steering` - 将文本列表转为 user 消息追加并触发上屏事件
- `%maybe-inject-steering!` - 在请求前尝试轮询并注入 steering 引导消息
- `%queue-before-stop` - 在单轮自然停止前检查指定队列并注入消息
- `%natural-stop-continue` - 停止前优先检测 steering，其次检测 follow-up，决定是否继续循环
