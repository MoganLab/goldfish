# (liii date-time current-time) 模块函数分类索引

`(liii date-time current-time)` 提供读取系统当前时间的入口函数：
单调时钟、系统时间戳、UTC 时间戳与物理时刻 moment。

## 常见用法示例

```scheme
(import (liii date-time current-time))

(current-monotonic-seconds)  ; => 单调递增的精确整数秒
(current-monotonic-millis)   ; => 单调递增的精确整数毫秒
(current-utc-timestamp)      ; => UTC 时区的 timestamp
(current-system-timestamp)   ; => 本地时区的 timestamp
(current-moment)             ; => 当前物理时刻 moment
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/current-time "函数名"
```

## 函数分类索引

### 一、单调时钟
- `current-monotonic-seconds` - 获取系统自开机以来的单调秒数
- `current-monotonic-millis` - 获取系统自开机以来的单调毫秒数

### 二、当前时间戳
- `current-system-timestamp` - 获取系统当前本地时区下的时间戳
- `current-utc-timestamp` - 获取系统当前 UTC 时区下的时间戳

### 三、物理时刻
- `current-moment` - 获取系统当前的物理时刻

## 错误处理约定

- 系统时钟不可用时抛出底层操作系统错误
