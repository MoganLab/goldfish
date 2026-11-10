# (liii http async) 模块函数索引

`(liii http async)` 提供异步 HTTP 客户端 API，基于 cpr 库实现，支持 GET/POST/HEAD 异步请求，通过回调函数处理响应，并提供 `http-poll`/`http-wait-all` 管理异步请求的生命周期。

使用时导入：

```scheme
(import (liii http async))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc http-async-get
```

## 函数分类索引

### 异步 HTTP 请求
- `http-async-get` - 异步 GET 请求
- `http-async-post` - 异步 POST 请求
- `http-async-head` - 异步 HEAD 请求

### 异步请求管理
- `http-poll` - 轮询并执行已完成的异步请求回调
- `http-wait-all` - 等待所有异步请求完成

### 响应处理
- `http-ok?` - 检查响应是否成功（2xx 状态码），实现见 `(liii http common)`
