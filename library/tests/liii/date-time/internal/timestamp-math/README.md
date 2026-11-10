# (liii date-time internal timestamp-math) 模块函数分类索引

`(liii date-time internal timestamp-math)` 实现时间运算的底层算法：
时间间隔换算、物理时刻推移与纯时钟加法，
是 (liii date-time timestamp) 时间运算函数的实现库。

## 常见用法示例

```scheme
(import (liii date-time time-delta))

(time-delta->seconds (time-delta+ (hours-delta 1) (minutes-delta 30)))
; => 5400
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-math "函数名"
```

## 函数分类索引

### 一、间隔换算
- `time-delta->seconds` - 将时间间隔的天/时/分/秒换算为总秒数

### 二、时刻运算
- `moment+seconds` - 将物理时刻向前或向后推移给定秒数
- `timestamp+/no-rules` - 对时间戳做纯时钟加法，不考虑时区规则
- `adjust-date-and-time` - 对时间戳应用日历语义的时间调整

## 错误处理约定

- 参数类型不符合要求时抛出 `type-error`
