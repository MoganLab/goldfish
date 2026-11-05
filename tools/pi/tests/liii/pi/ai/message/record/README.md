# (liii pi ai message record) 模块函数索引

`(liii pi ai message record)` 提供面向业务的消息与内容块构造器、角色判定、类型提取及转换为 alist 的接口。

使用时导入：

```scheme
(import (liii pi ai message record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-user-message
```

## 函数分类索引

### 消息构造与转换
- `pi-message?` - 判断是否为消息对象
- `pi-message-role` - 获取消息角色
- `pi-message->alist` - 消息对象转 alist
- `pi-user-message` - 构造用户消息
- `pi-user-message-role` - 获取用户消息角色
- `pi-assistant-message` - 构造助手消息
- `pi-assistant-message-role` - 获取助手消息角色
- `pi-tool-result-message` - 构造工具结果消息
- `pi-tool-result-message-role` - 获取工具结果消息角色

### 内容块构造与转换
- `pi-block?` - 判断是否为内容块
- `pi-block-type` - 获取内容块类型
- `pi-block->alist` - 内容块转 alist
- `pi-text-block` - 构造文本块
- `pi-text-block-type` - 获取文本块类型
- `pi-thinking-block` - 构造思考块
- `pi-thinking-block-type` - 获取思考块类型
- `pi-tool-call-block` - 构造工具调用块
- `pi-tool-call-block-type` - 获取工具调用块类型
- `pi-image-block` - 构造图片块
- `pi-image-block-type` - 获取图片块类型
- `pi-bash-execution->user-text` - 构造 Bash 执行结果注入文本
