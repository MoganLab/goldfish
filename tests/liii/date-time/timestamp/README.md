# (liii date-time timestamp) 模块函数分类索引

`(liii date-time timestamp)` 提供带时区时间戳对象的构造、字段读取、比较与运算。

## 常见用法示例

```scheme
(import (liii date-time timestamp))

(timestamp-year (make-timestamp 2026 1 2 3 4 5 (utc-timezone))) ; => 2026
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/timestamp "函数名"
```

## 函数索引

- `make-timestamp`
- `date+clock-time->timestamp` - 通过 date 对象和 clock-time 对象组合创建 timestamp。
- `timestamp?`
- `timestamp-date`
- `timestamp-clock-time`
- `timestamp-timezone`
- `timestamp-fold`
- `timestamp-year`
- `timestamp-month`
- `timestamp-day`
- `timestamp-ymd`
- `timestamp-hour`
- `timestamp-minute`
- `timestamp-second`
- `timestamp-hms`
- `moment->timestamp`
- `timestamp->moment`
- `posix-time->utc-timestamp`
- `timestamp->utc-posix-time`
- `timestamp-in-timezone`
- `timestamp-timezone-offset`
- `timestamp-timezone-dst?`
- `timestamp-timezone-abbreviation`
- `timestamp->iso-8601`
- `iso-8601->timestamp`
- `timestamp+`
- `timestamp-`
- `timestamp=?` - 比较两个或多个时间戳在物理绝对时间上是否等价。
- `timestamp<?` - 比较时间戳是否严格单调递增（小于）。
- `timestamp<=?` - 比较时间戳是否单调递增（小于或等于）。
- `timestamp>?` - 比较时间戳是否严格单调递减（大于）。
- `timestamp>=?` - 比较时间戳是否单调递减（大于或等于）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
