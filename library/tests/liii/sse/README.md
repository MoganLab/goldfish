# (liii sse) 模块函数分类索引

`(liii sse)` 提供 Server-Sent Events (SSE) 事件流的编解码与分帧处理能力，适合大语言模型 (LLM) 流式调用、实时长连接通知推送与网络数据分片解析。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/sse "函数名"
```

## 函数分类索引

### 一、核心记录类型与访问器
- `make-sse-event` - 构造 SSE 事件记录对象
- `sse-event?` - 判断对象是否为 SSE 事件记录
- `sse-event-event` - 获取事件类型（默认 "message"）
- `sse-event-data` - 获取事件负载数据字符串
- `sse-event-id` - 获取事件 ID 或 #f
- `sse-event-retry` - 获取重试间隔毫秒数或 #f
- `sse-event-json` - 将事件负载数据反序列化为 JSON 结构

### 二、流式读取与增量解码
- `read-sse-event` - 从输入端口消费并解析下一个完整 SSE 事件
- `make-sse-decoder` - 构造增量 Chunk 分片解码器闭包

### 三、事件序列化与写出
- `sse-event->string` - 将事件记录序列化为 SSE 格式文本字符串
- `write-sse-event` - 将事件记录序列化并写出到输出端口
