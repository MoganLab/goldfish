# (scheme time) 模块函数分类索引

`(scheme time)` 提供 R7RS 的时间查询函数与 Goldfish 扩展的时钟设施，
涵盖当前时间读取、计时单元换算和单调时钟测量，适合性能计时与时间戳生成。

## 常见用法示例

```scheme
(import (scheme time))

;; 示例1：测量代码耗时
(define start (current-jiffy))
;; ... 被计时的代码 ...
(/ (- (current-jiffy) start) (jiffies-per-second))

;; 示例2：读取当前时间（秒 + 微秒）
(get-time-of-day)              ; => 两个值：sec usec

;; 示例3：单调时钟纳秒计数（不受系统时间调整影响）
(monotonic-nanosecond)         ; => 递增的整数
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/time "函数名"
```

## 函数分类索引

### 一、当前时间

- `current-second` - 获取当前时间（秒，浮点数）
- `get-time-of-day` - 获取当前时间的整秒数与微秒余数
- `monotonic-nanosecond` - 获取单调时钟的纳秒计数

### 二、计时单元

- `current-jiffy` - 获取当前时间的 jiffy 计数
- `jiffies-per-second` - 每秒对应的 jiffy 数

### 三、时钟分辨率

- `system-clock-resolution` - 系统时钟分辨率（纳秒）
- `steady-clock-resolution` - 稳定时钟分辨率（纳秒）
