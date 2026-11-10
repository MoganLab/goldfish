# (liii pi ai providers quota) 模块函数索引

`(liii pi ai providers quota)` 提供服务商额度耗尽（quota / usage limit）识别的共享工具与通用数据结构，包括候选错误标识提取、码表匹配以及错误消息提取。

使用时导入：

```scheme
(import (liii pi ai providers quota))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-quota-error-result
```

## 函数分类索引

### 结构与结果判定
- `pi-quota-limit?` - 判断是否为配额限制判定结果
- `pi-quota-error-result` - 通用状态码与码表配额错误匹配判定
- `set-pi-quota-error-logger!` - 设置配额解析异常日志记录器

### JSON 错误提取与匹配
- `pi-quota-json-fields` - 从错误响应中提取候选标识与消息
- `pi-quota-match-codes` - 候选标识与服务商码表精确匹配
- `pi-quota-error-message` - 安全提取响应体中的错误消息
- `pi-quota-message-trailing-code` - 从错误消息末尾提取数字业务码
