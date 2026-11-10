# (liii pi code modes output) 模块函数分类索引

`(liii pi code modes output)` 提供 print 模式输出的格式化能力，覆盖消息文本提取、错误结算与结果输出。

使用时导入：

```scheme
(import (liii pi code modes output))
```

## 查看函数文档

```bash
gf doc display-stderr
gf doc handle-print-result
gf doc pi-content-text
```

## 函数分类索引

### 消息文本
- `pi-content-text` - 提取消息 content 字段中的文本内容
- `pi-message-field` - 读取消息中的指定字段
- `pi-last-message` - 返回消息列表中的最后一条消息
- `pi-last-assistant-text` - 返回最后一条非空白文本内容

### 结果输出
- `display-stderr` - 向标准错误输出一行文本
- `handle-print-result` - 输出 print 模式的最终结果并返回退出状态码
