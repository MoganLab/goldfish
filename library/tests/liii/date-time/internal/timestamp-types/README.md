# (liii date-time internal timestamp-types) 模块函数分类索引

`(liii date-time internal timestamp-types)` 定义 timestamp 记录类型与全部字段访问器的底层实现库。

## 常见用法示例

```scheme
(import (liii date-time internal timestamp-types))

(timestamp-year ts) ; => integer?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-types "函数名"
```

## 函数索引

- `%make-timestamp` - 不校验时区规则直接构造 timestamp 记录。
- `timestamp?` - 判断给定对象是否为 timestamp 对象。
- `%timestamp-date` - 读取 timestamp 记录的日期字段。
- `%timestamp-clock-time` - 读取 timestamp 记录的时钟时间字段。
- `%timestamp-timezone` - 读取 timestamp 记录的时区字段。
- `%timestamp-fold` - 读取 timestamp 记录的 fold 折叠字段。
- `timestamp-date` - 获取时间戳对象的公历日期部分。
- `timestamp-clock-time` - 获取时间戳对象的时钟时间部分。
- `timestamp-timezone` - 获取时间戳对象的时区对象。
- `timestamp-fold` - 获取时间戳对象的 fold 标志（0 或 1）。
- `timestamp-ymd` - 以多值（values）形式返回时间戳对象的年、月、日。
- `timestamp-year` - 获取时间戳对象的年份。
- `timestamp-month` - 获取时间戳对象的月份。
- `timestamp-day` - 获取时间戳对象的日。
- `timestamp-hms` - 以多值（values）形式返回时间戳对象的时、分、秒。
- `timestamp-hour` - 获取时间戳对象的小时数。
- `timestamp-minute` - 获取时间戳对象的分钟数。
- `timestamp-second` - 获取时间戳对象的秒数（精确有理数或整数）。
- `timestamp->local-timepoint` - 将时间戳的本地日期时间换算为自 UNIX 纪元起的秒数（忽略时区偏移）。
- `timestamp-timezone-offset` - 获取时间戳在当前时区下的 UTC 偏移量（作为 time-delta 返回）。
- `timestamp-timezone-abbreviation` - 获取时间戳在当前时区下的缩写名称（如 "EDT", "EST", "UTC"）。
- `timestamp-timezone-dst?` - 判断时间戳当前是否处于夏令时（DST）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
