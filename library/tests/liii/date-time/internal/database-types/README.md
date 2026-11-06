# (liii date-time internal database-types) 模块函数分类索引

`(liii date-time internal database-types)` 定义 time-range 与 timezone 两个核心记录类型及其访问器。

## 常见用法示例

```scheme
(import (liii date-time internal database-types))

(make-timezone first utc-ranges local-ranges rule)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/database-types "函数名"
```

## 函数索引

- `time-range` - 构造时区时间区间记录。
- `time-range?` - 判断对象是否为时间区间记录。
- `time-range-start` - 读取时间区间的起点字段。
- `time-range-end` - 读取时间区间的终点字段。
- `time-range-offset` - 读取时间区间的 UTC 偏移字段。
- `time-range-dst?` - 读取时间区间的夏令时标志字段。
- `time-range-designation` - 读取时间区间的时区缩写字段。
- `make-timezone` - 构造时区数据记录。
- `timezone?` - 判断给定对象是否为时区对象。
- `timezone-first` - 读取时区记录的首个区间字段。
- `timezone-utc-ranges` - 读取时区记录的 UTC 区间向量字段。
- `timezone-local-ranges` - 读取时区记录的本地区间向量字段。
- `timezone-rule` - 读取时区记录的 POSIX 回退规则字段。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
