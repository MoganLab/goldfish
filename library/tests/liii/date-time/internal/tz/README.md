# (liii date-time internal tz) 模块函数分类索引

`(liii date-time internal tz)` 解析 POSIX TZ 字符串为 tz-string 记录，供 DST 回退规则使用。

## 常见用法示例

```scheme
(import (liii date-time internal tz))

(parse-tz "EST5EDT,M3.2.0,M11.1.0") ; => tz-string?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/tz "函数名"
```

## 函数索引

- `tz-offset`
- `tz-offset?`
- `tz-offset-sign-negative?`
- `tz-offset-hour`
- `tz-offset-minute`
- `tz-offset-second`
- `tz-month-week-day`
- `tz-month-week-day?`
- `tz-month-week-day:month`
- `tz-month-week-day:week`
- `tz-month-week-day:day`
- `tz-julian-without-leap-day`
- `tz-julian-without-leap-day?`
- `tz-julian-without-leap-day:date`
- `tz-julian-with-leap-day`
- `tz-julian-with-leap-day?`
- `tz-julian-with-leap-day:date`
- `tz-transition`
- `tz-transition?`
- `tz-transition-date`
- `tz-transition-time`
- `tz-string`
- `tz-string?`
- `tz-string-stdname`
- `tz-string-stdoffset`
- `tz-string-dstname`
- `tz-string-dstoffset`
- `tz-string-change-to-dst`
- `tz-string-change-to-std`
- `parse-tz` - 解析 POSIX TZ 字符串为 tz-string 记录。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
