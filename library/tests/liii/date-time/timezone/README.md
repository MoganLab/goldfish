# (liii date-time timezone) 模块函数分类索引

`(liii date-time timezone)` 提供时区查询接口：IANA 时区、系统时区、固定偏移时区与偏移查询。

## 常见用法示例

```scheme
(import (liii date-time timezone))

(tz-timezone "Asia/Shanghai") ; => timezone?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/timezone "函数名"
```

## 函数索引

- `timezone?`
- `utc-timezone`
- `utc-offset-timezone`
- `system-timezone`
- `tz-timezone`
- `timezone-earliest`
- `timezone-latest`
- `timezone-offset` - 查询时区在指定 UTC 时刻的偏移。
- `timezone-dst?` - 查询时区在指定 UTC 时刻是否处于夏令时（DST）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
