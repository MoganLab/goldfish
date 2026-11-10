# (liii date-time internal database-ranges) 模块函数分类索引

`(liii date-time internal database-ranges)` 由 TZif 转移数据构造 UTC/本地时间区间向量并按时刻查询偏移。

## 常见用法示例

```scheme
(import (liii date-time internal database-ranges))

(find-offset/utc tz unix-time) ; => time-range?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/database-ranges "函数名"
```

## 函数索引

- `build-ranges` - 由 TZif 转移向量构造 UTC 与本地两套时间区间向量。
- `find-offset/utc` - 按 UNIX 时刻查找时区中对应的时间区间。
- `find-offset/wall` - 按本地墙钟时刻查找时区中匹配的时间区间集合。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
