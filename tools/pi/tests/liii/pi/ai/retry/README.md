# (liii pi ai retry) 模块函数索引

`(liii pi ai retry)` 提供 API 请求的错误重试机制，包括网络错误与服务商限流状态码判定、指数退避延迟计算、Retry-After 头部解析以及重试日志拦截。

使用时导入：

```scheme
(import (liii pi ai retry))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-is-retriable-error?
```

## 函数分类索引

### 可重试错误判定
- `pi-is-retriable-error?` - 判断 HTTP 状态码与响应是否属于可重试错误
- `gemini-is-retriable-error?` - Gemini 请求错误重试判定（别名）
- `anthropic-is-retriable-error?` - Anthropic 请求错误重试判定（别名）

### 延迟计算与参数解析
- `pi-calc-retry-delay` - 计算指数退避重试延迟秒数
- `pi-resolve-retry-params` - 解析配置中的重试参数与预算

### Retry-After 解析
- `pi-extract-retry-after` - 从响应对象提取 Retry-After 头部字符串
- `pi-parse-retry-after` - 解析秒数或 HTTP 日期格式的等待时间

### 日志记录
- `pi-log-retry` - 记录请求重试等待日志
- `set-pi-retry-logger!` - 设置自定义重试日志记录器
- `pi-ai-log!` - AI 层通用日志输出通道
