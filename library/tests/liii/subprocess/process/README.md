# (liii subprocess process) 模块函数分类索引

`(liii subprocess process)` 模块提供底层子进程的创建、生命周期管理、I/O 通信与活动状态监控，以及选项参数解析和归一化校验辅助机制。

## 查看函数文档

使用以下命令查看子模块文档：
```bash
gf doc liii/subprocess/process
```
或者查看具体函数的文档和用例：
```bash
gf doc liii/subprocess/process "process-spawn"
```

## 函数分类索引

### 一、进程生命周期管理
- `process?` - 判定给定对象是否为子进程对象
- `process-pid` - 获取子进程的操作系统 PID
- `process-spawn` - 创建并启动底层子进程
- `process-alive?` - 判定子进程当前是否存活
- `process-poll` - 非阻塞查询子进程退出状态码
- `process-wait` - 阻塞或带超时等待子进程退出并返回状态码
- `process-terminate` - 发送 SIGTERM 优雅终止子进程
- `process-kill` - 发送 SIGKILL 强制终止子进程
- `process-kill-tree` - 强制终止子进程及其所有派生子进程树
- `process-close` - 关闭并释放子进程对象及相关资源

### 二、I/O 与活动状态监控
- `process-write` - 向子进程标准输入写入文本或字节数据
- `process-write-stdin` - 向子进程标准输入写入数据
- `process-close-stdin` - 关闭子进程标准输入管道
- `process-read` - 读取子进程标准输出或标准错误数据
- `process-read-line` - 从子进程输出中逐行读取文本数据
- `process-poll-io` - 阻塞式轮询子进程的 I/O 事件
- `process-eof?` - 判定子进程指定输出流是否已到达末尾
- `process-last-activity-time` - 获取子进程最后一次 I/O 活动的时间戳
- `process-idle-seconds` - 获取子进程距离最后一次活动的空闲秒数

### 三、内部辅助解析与校验
- `%keyword-value` - 从参数列表中提取指定关键字参数的值
- `%valid-env?` - 校验环境变量关联列表结构与类型合法性
- `%normalize-stream-spec` - 校验并归一化重定向流规格参数
- `%normalize-cwd` - 校验并归一化工作目录参数
