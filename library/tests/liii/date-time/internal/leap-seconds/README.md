# (liii date-time internal leap-seconds) 模块函数分类索引

`(liii date-time internal leap-seconds)` 内置 IERS 闰秒表，提供按 UTC/TAI 秒查询闰秒与 TAI 偏移。

## 常见用法示例

```scheme
(import (liii date-time internal leap-seconds))

(leapsecond-info/utc 63072000) ; => #t, 10
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/leap-seconds "函数名"
```

## 函数索引

- `default-leap-seconds` - 内置的 IERS 闰秒表，按 UNIX 时间升序排列。
- `leapsecond-info/tai` - 按 TAI 秒查询闰秒信息，返回是否闰秒与当前 TAI 偏移。
- `leapsecond-info/utc` - 按 UTC 秒查询闰秒信息，返回是否闰秒与当前 TAI 偏移。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
