# (liii pi agent loop-retry) 模块函数索引

`(liii pi agent loop-retry)` 提供 Agent 主循环中的请求超时识别与重试调度机制，当模型请求超时时进行重试，并在重试提示信息中明确体现超时秒数。

使用时导入：

```scheme
(import (liii pi agent loop-retry))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-request-with-timeout-retry
```

## 函数分类索引

### 超时重试
- `pi-request-with-timeout-retry` - 请求带超时自动重试及秒数提示调度
