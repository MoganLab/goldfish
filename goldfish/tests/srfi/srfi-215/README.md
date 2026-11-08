# (srfi srfi-215) 模块函数分类索引

SRFI-215 定义「中央日志交换」（Central Log Exchange）：
应用把日志消息交给一个可全局替换的回调过程，由应用决定日志最终写到哪里。

日志级别沿用 RFC 5424 Syslog 协议的取值：`EMERGENCY`(0) 最严重，`DEBUG`(7) 最轻微。

## 常见用法示例

```scheme
(import (srfi srfi-215))

;; 示例1：安装日志回调
(current-log-callback (lambda (entry) (display entry) (newline)))

;; 示例2：发送一条日志
(send-log WARNING "disk full" 'DISK "/var")

;; 示例3：设置全局默认字段
(current-log-fields '(FACILITY 1))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-215 "函数名"
```

## 函数分类索引

### 一、发送日志
- `send-log` - 发送一条日志消息，附带可选的附加字段

### 二、全局状态
- `current-log-fields` - 读取或设置全局默认日志字段
- `current-log-callback` - 读取或设置当前日志回调过程

### 三、日志级别
- `EMERGENCY` - 系统不可用（0）
- `ALERT` - 必须立即处理（1）
- `CRITICAL` - 严重错误（2）
- `ERROR` - 错误（3）
- `WARNING` - 警告（4）
- `NOTICE` - 通知（5）
- `INFO` - 信息（6）
- `DEBUG` - 调试（7）
