# (liii logging) 模块函数分类索引

`(liii logging)` 是与 SRFI-215 兼容的日志模块，提供便捷函数、级别控制、格式化输出与文件处理器。

## 查看函数文档

```bash
gf doc liii/logging
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/logging "send-log"
```

## 函数分类索引

### 一、级别常量
- `EMERGENCY` - 级别常量：系统已不可用（0）
- `ALERT` - 级别常量：必须立即采取行动（1）
- `CRITICAL` - 级别常量：严重故障（2）
- `ERROR` - 级别常量：错误（3）
- `WARNING` - 级别常量：警告（4）
- `NOTICE` - 级别常量：普通提示（5）
- `INFO` - 级别常量：一般信息（6）
- `DEBUG` - 级别常量：调试信息（7）

### 二、发送日志
- `send-log` - 按指定严重程度发送一条日志消息（SRFI-215 兼容入口）
- `log-emergency` - 以 EMERGENCY 级别发送日志
- `log-alert` - 以 ALERT 级别发送日志
- `log-critical` - 以 CRITICAL 级别发送日志
- `log-error` - 以 ERROR 级别发送日志
- `log-warning` - 以 WARNING 级别发送日志
- `log-notice` - 以 NOTICE 级别发送日志
- `log-info` - 以 INFO 级别发送日志
- `log-debug` - 以 DEBUG 级别发送日志

### 三、级别与格式控制
- `log-set-level!` - 设置日志级别阈值
- `current-log-level` - 读取当前日志级别阈值
- `log-set-format!` - 设置日志格式串
- `current-log-format` - 读取当前日志格式串

### 四、全局字段与回调
- `log-set-fields!` - 设置全局日志字段（plist）
- `current-log-fields` - 读取当前全局日志字段
- `current-log-fields-in-alist` - 用 alist 形态设置全局日志字段
- `log-set-callback!` - 设置日志回调
- `current-log-callback` - 读取当前日志回调

### 五、处理器
- `default-log-handler` - 按当前格式把一条日志写入指定端口
- `make-stdout-handler` - 创建输出到标准输出流的处理器
- `make-stderr-handler` - 创建输出到标准错误流的处理器
- `make-file-handler` - 创建追加写入文件的处理器
- `log-set-file-handler!` - 把日志回调切换为文件处理器
- `log-flush!` - 立即刷新日志文件端口

### 六、消息字段读取
- `log-message-severity` - 读取日志消息的严重程度
- `log-message-message` - 读取日志消息的正文
- `log-message-field` - 按键读取日志消息中的附加字段
