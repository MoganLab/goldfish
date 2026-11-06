# (liii date-time internal tz-types) 模块函数分类索引

`(liii date-time internal tz-types)` 定义 POSIX TZ 语法涉及的 tz-offset 等记录类型与访问器。

## 常见用法示例

```scheme
(import (liii date-time internal tz-types))

(tz-offset #f 5 30 0) ; => tz-offset?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/tz-types "函数名"
```

## 函数索引

- `tz-offset` - 构造 POSIX 时区偏移记录。
- `tz-offset?` - 判断对象是否为 POSIX 时区偏移记录。
- `tz-offset-sign-negative?` - 读取偏移记录的负号标志字段。
- `tz-offset-hour` - 读取偏移记录的小时字段。
- `tz-offset-minute` - 读取偏移记录的分钟字段。
- `tz-offset-second` - 读取偏移记录的秒字段。
- `tz-month-week-day` - 构造 POSIX「月-周-星期」转移日期记录。
- `tz-month-week-day?` - 判断对象是否为月-周-星期记录。
- `tz-month-week-day:month` - 读取月-周-星期记录的月份字段。
- `tz-month-week-day:week` - 读取月-周-星期记录的周数字段。
- `tz-month-week-day:day` - 读取月-周-星期记录的星期字段。
- `tz-julian-without-leap-day` - 构造忽略闰日的儒略日转移记录（1-based 年内日序）。
- `tz-julian-without-leap-day?` - 判断对象是否为忽略闰日的儒略日记录。
- `tz-julian-without-leap-day:date` - 读取忽略闰日的儒略日记录的日序字段。
- `tz-julian-with-leap-day` - 构造考虑闰日的儒略日转移记录（0-based 年内日序）。
- `tz-julian-with-leap-day?` - 判断对象是否为考虑闰日的儒略日记录。
- `tz-julian-with-leap-day:date` - 读取考虑闰日的儒略日记录的日序字段。
- `tz-transition` - 构造 POSIX 转移日期与时刻记录。
- `tz-transition?` - 判断对象是否为 POSIX 转移记录。
- `tz-transition-date` - 读取转移记录的日期规则字段。
- `tz-transition-time` - 读取转移记录的时刻字段。
- `tz-string` - 构造 POSIX TZ 字符串解析结果记录。
- `tz-string?` - 判断对象是否为 POSIX TZ 定义记录。
- `tz-string-stdname` - 读取 POSIX TZ 记录的标准时间缩写字段。
- `tz-string-stdoffset` - 读取 POSIX TZ 记录的标准时间偏移字段。
- `tz-string-dstname` - 读取 POSIX TZ 记录的夏令时缩写字段。
- `tz-string-dstoffset` - 读取 POSIX TZ 记录的夏令时偏移字段。
- `tz-string-change-to-dst` - 读取 POSIX TZ 记录的进入夏令时转移字段。
- `tz-string-change-to-std` - 读取 POSIX TZ 记录的退出夏令时转移字段。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
