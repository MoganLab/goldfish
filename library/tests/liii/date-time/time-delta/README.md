# (liii date-time time-delta) 模块函数分类索引

`(liii date-time time-delta)` 提供时间增量（年/月/周/日/时/分/秒）的构造、分量读取与算术运算。

## 常见用法示例

```scheme
(import (liii date-time time-delta))

(time-delta-days (days-delta 10)) ; => 10
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/time-delta "函数名"
```

## 函数索引

- `make-time-delta` - 创建一个时间增量（time-delta）对象。
- `time-delta?` - 判断给定对象是否为 time-delta 对象。
- `time-delta-years` - 获取 time-delta 的年增量。
- `time-delta-months` - 获取 time-delta 的月增量。
- `time-delta-weeks` - 获取 time-delta 的周增量。
- `time-delta-days` - 获取 time-delta 的日增量。
- `time-delta-hours` - 获取 time-delta 的小时增量。
- `time-delta-minutes` - 获取 time-delta 的分钟增量。
- `time-delta-seconds` - 获取 time-delta 的秒增量（精确有理数或整数）。
- `years-delta` - 创建仅包含年份增量的 time-delta 对象。
- `months-delta` - 创建仅包含月份增量的 time-delta 对象。
- `weeks-delta` - 创建仅包含周数增量的 time-delta 对象。
- `days-delta` - 创建仅包含天数增量的 time-delta 对象。
- `hours-delta` - 创建仅包含小时增量的 time-delta 对象。
- `minutes-delta` - 创建仅包含分钟增量的 time-delta 对象。
- `seconds-delta` - 创建仅包含秒数增量的 time-delta 对象。
- `time-delta+` - 将多个 time-delta 对象的各个时间分量对应相加。
- `time-delta-` - 计算 time-delta 的差值或取负。
- `time-delta-negate` - 将 time-delta 对象的所有分量取反。
- `time-delta=?` - 判断两个 time-delta 是否逐分量相等。
- `date+` - 将日期与时间增量相加。
- `date-` - 计算日期减去时间增量。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
