# (liii date-time date) 模块函数分类索引

`(liii date-time date)` 提供历法日期对象的构造、字段读取、比较、ISO 周与儒略日/raté die 换算。

## 常见用法示例

```scheme
(import (liii date-time date))

(date-year (make-date 2021 1 2)) ; => 2021
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/date "函数名"
```

## 函数索引

- `make-date`
- `date?`
- `date-year`
- `date-month`
- `date-day`
- `date-ymd`
- `date->iso-8601` - 将日期对象转换为 ISO-8601 格式的字符串（YYYY-MM-DD）。
- `iso-8601->date` - 将符合 ISO-8601 格式的日期字符串解析为日期对象。
- `date-iso-weekday` - 获取日期对象的 ISO 星期几（1-7，周一为 1，周日为 7）。
- `date-iso-week` - 获取日期所在的 ISO 周数（1-53）。
- `date-iso-week-year` - 获取日期所属的 ISO 周对应的年份（ISO week-numbering year）。
- `date->rata-die` - 将日期转换为 Rata Die 日序数（公历 0001-01-01 为 Day 1）。
- `rata-die->date` - 将 Rata Die 日序数转换为公历日期对象。
- `date->mjd` - 将日期转换为简化儒略日（Modified Julian Day, MJD）。
- `mjd->date` - 将简化儒略日（MJD）转换为公历日期对象。
- `date=?` - 比较两个或多个日期对象是否相等。
- `date<?` - 比较日期对象是否严格单调递增（小于）。
- `date<=?` - 比较日期对象是否单调递增（小于或等于）。
- `date>?` - 比较日期对象是否严格单调递减（大于）。
- `date>=?` - 比较日期对象是否单调递减（大于或等于）。
- `leap-year?`
- `days-in-month-raw`

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
