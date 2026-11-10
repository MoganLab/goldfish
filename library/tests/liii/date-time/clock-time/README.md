# (liii date-time clock-time) 模块函数分类索引

`(liii date-time clock-time)` 提供时钟时间（一天中的时分秒）对象的构造、字段读取、比较与 ISO-8601 转换。

## 常见用法示例

```scheme
(import (liii date-time clock-time))

(clock-time-hour (make-clock-time 23 1 2)) ; => 23
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/clock-time "函数名"
```

## 函数索引

- `make-clock-time` - 创建一天内的时钟时间对象（clock-time）。
- `clock-time?` - 判断给定对象是否为 clock-time 对象。
- `clock-time-hour` - 获取时钟时间对象的小时数。
- `clock-time-minute` - 获取时钟时间对象的分钟数。
- `clock-time-second` - 获取时钟时间对象的秒数（精确有理数或整数）。
- `clock-time-hms` - 以多值（values）形式返回时钟时间对象的时、分、秒。
- `clock-time->iso-8601` - 将时钟时间对象转换为 ISO-8601 格式的时间字符串（如 "HH:MM:SS" 或 "HH:MM:SS.sss"）。
- `iso-8601->clock-time` - 将符合 ISO-8601 格式的时间字符串解析为 clock-time 对象。
- `clock-time=?` - 比较两个或多个 clock-time 是否相等。
- `clock-time<?` - 比较 clock-time 是否严格单调递增（小于）。
- `clock-time<=?` - 比较 clock-time 是否单调递增（小于或等于）。
- `clock-time>?` - 比较 clock-time 是否严格单调递减（大于）。
- `clock-time>=?` - 比较 clock-time 是否单调递减（大于或等于）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
