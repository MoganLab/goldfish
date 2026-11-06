# (liii date-time internal timestamp-conv) 模块函数分类索引

`(liii date-time internal timestamp-conv)` 实现时间戳与 moment、UNIX 秒、时区及加减运算之间的转换。

## 常见用法示例

```scheme
(import (liii date-time internal timestamp-conv))

(timestamp+ ts (hours-delta 1)) ; => timestamp?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-conv "函数名"
```

## 函数索引

- `timestamp+` - 将时间戳与时间增量相加，正确处理夏令时跃变、折叠与闰秒。
- `timestamp-` - 计算时间戳减去时间增量。
- `timestamp-in-timezone` - 将时间戳切换为指定时区表示的同等时刻。
- `moment->timestamp` - 将时刻（moment）对象转换为指定时区下的时间戳（timestamp）。
- `timestamp->moment` - 将时间戳对象转换为时刻（moment）对象。
- `posix-time->utc-timestamp` - 将 POSIX 时间（从 1970-01-01T00:00:00Z 起算的秒数）转换为 UTC 时间戳。
- `timestamp->utc-posix-time` - 将时间戳转换为对应的 UTC POSIX 秒数。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
