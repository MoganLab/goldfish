# (liii date-time internal date-types) 模块函数分类索引

`(liii date-time internal date-types)` 定义 date 记录类型、字段访问器与闰年/每月天数基础函数。

## 常见用法示例

```scheme
(import (liii date-time internal date-types))

(make-date 2021 1 2) ; => date?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/date-types "函数名"
```

## 函数索引

- `%make-date` - 不校验参数直接构造 date 记录。
- `date?` - 判断给定对象是否为 (liii date-time date) 的日期对象。
- `%date-year` - 读取 date 记录的年份字段。
- `%date-month` - 读取 date 记录的月份字段。
- `%date-day` - 读取 date 记录的日字段。
- `make-date` - 创建一个公历日期对象。
- `date-year` - 获取日期对象的年份。
- `date-month` - 获取日期对象的月份。
- `date-day` - 获取日期对象的日。
- `date-ymd` - 以多值（values）形式返回日期对象的年、月、日。
- `leap-year?` - 判断给定年份在公历中是否为闰年。
- `days-in-month-raw` - 返回给定年份类型与月份的天数，非法月份返回 #f。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
