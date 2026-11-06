# (liii date-time internal database) 模块函数分类索引

`(liii date-time internal database)` 加载并缓存 TZif 时区数据，是 tz-timezone 等接口的底层实现库。

## 常见用法示例

```scheme
(import (liii date-time internal database))

(tz-timezone "UTC") ; => timezone?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/database "函数名"
```

## 函数索引

- `timezone?`
- `time-range`
- `time-range?`
- `time-range-start`
- `time-range-end`
- `time-range-offset`
- `time-range-dst?`
- `time-range-designation`
- `timezone-first`
- `timezone-utc-ranges`
- `timezone-local-ranges`
- `timezone-rule`
- `timezone-earliest` - 获取该时区支持的最早时刻（moment）。
- `timezone-latest` - 获取该时区支持的最晚时刻（moment）。
- `find-offset/wall`
- `find-offset/utc`
- `utc-timezone` - 获取 UTC 协调世界时时区对象。
- `utc-offset-timezone` - 根据固定时间偏移量创建时区对象。
- `system-timezone` - 获取操作系统当前本地时区对象。
- `tz-timezone` - 根据 IANA 时区名称（如 "Europe/Vilnius", "America/New_York", "Asia/Shanghai"）获取时区对象。
- `tz-timezones` - 获取系统所有可用的 IANA 时区名称列表。
- `reload-timezone-data!` - 清空已缓存的时区对象和时区名称列表，重新加载时区数据。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
