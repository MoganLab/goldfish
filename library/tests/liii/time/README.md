# (liii time) 模块函数分类索引

`(liii time)` 类似于 Python 的 `time` 模块，主要用于休眠等基础系统时钟操作。日期与时间高级处理请使用 `(liii date-time)`。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/time "函数名"
```

## 函数分类索引

### 一、休眠
- `sleep` - 使当前线程暂停执行指定的秒数

### 二、进程 CPU 时间
- `process-time` - 当前进程累计 CPU 时间（秒）
- `process-time-ns` - 当前进程累计 CPU 时间（纳秒）

### 三、线程 CPU 时间
- `thread-time` - 当前操作系统线程累计 CPU 时间（秒）
- `thread-time-ns` - 当前操作系统线程累计 CPU 时间（纳秒）
