# (liii http) 模块函数索引

`(liii http)` 提供同步 HTTP 客户端 API，基于 cpr 库实现，支持 GET/POST/HEAD 请求。其中 `http-get` 在 `:stream #t` 时可做简单流式下载，`http-post` 支持通过 `:files` 上传文件，并在 `:stream #t` 时按 chunk 处理响应体。异步 API 见 `(liii http async)` 模块。

使用时导入：

```scheme
(import (liii http))
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii http common)` | `(liii http)` 与 `(liii http async)` 共享的响应处理与参数校验辅助函数 |
| `(liii http async)` | 异步 HTTP 客户端 API（GET/POST/HEAD 回调与请求管理） |

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc http-get
```

## 函数分类索引

### 同步 HTTP 请求
- `http-head` - 发送 HEAD 请求，获取响应头
- `http-get` - 发送 GET 请求；`:stream #t` 时也支持简单流式下载
- `http-post` - 发送 POST 请求，也支持通过 `:files` 上传文件

### 响应处理
- `http-ok?` - 检查响应是否成功（2xx 状态码），实现见 `(liii http common)`
