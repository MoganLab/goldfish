# (liii date-time internal timestamp-construct) 模块函数分类索引

`(liii date-time internal timestamp-construct)` 实现时间戳构造、时区换算与闰秒校验。

## 常见用法示例

```scheme
(import (liii date-time internal timestamp-construct))

(make-timestamp 2026 1 2 3 4 5 tz) ; => timestamp?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-construct "函数名"
```

## 函数索引

- `make-timestamp` - 创建带时区的时间戳对象。
- `date+clock-time->timestamp` - 由日期、时钟时间与时区组装时间戳，并校验本地时间在时区内合法。
- `timestamp-in-utc` - 将时间戳换算为相同物理时刻的 UTC 时间戳。
- `ts-no-rules` - 不应用时区 DST 规则地给时间戳加上时间增量。
- `validate-leapsecond` - 校验时间戳的秒字段是否落在合法闰秒上，成功调用 ok、失败调用 err。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
