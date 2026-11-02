# (liii pi ai log) 模块函数索引

`(liii pi ai log)` 提供 pi AI 层的异常请求日志功能，在发生 429/403/5xx 等异常 HTTP 响应或网络故障时以 ERROR 级别将请求上下文及响应体原文写入日志文件。

使用时导入：

```scheme
(import (liii pi ai log))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-log-dir
```

## 函数分类索引

### 日志目录与文件
- `pi-log-dir` - 返回日志存放目录路径
- `pi-log-file-path` - 返回当前日期对应的日志文件路径
- `pi-log-init!` - 初始化日志目录
- `pi-log-reset!` - 关闭日志文件端口并重置状态
- `pi-log-enabled?` - 判断文件日志是否开启

### 日志写入
- `pi-log-error!` - 以 ERROR 级别写一条日志
- `pi-log-http-error!` - 记录异常 HTTP 请求及其响应原文
