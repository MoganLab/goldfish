# (liii date-time internal dst) 模块函数分类索引

`(liii date-time internal dst)` 实现 POSIX tzrule 记录与夏令时转移时刻、偏移区间的查找算法。

## 常见用法示例

```scheme
(import (liii date-time internal dst))

(add-offset 0 (hours-delta 1)) ; => 3600
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/dst "函数名"
```

## 函数索引

- `tzrule` - 构造 DST 规则记录。
- `tzrule?` - 判断对象是否为 DST 规则记录。
- `tzrule-std-name` - 读取规则的标准时间缩写字段。
- `tzrule-std-offset` - 读取规则的标准时间偏移字段。
- `tzrule-dst-transition` - 读取规则进入夏令时的转移字段。
- `tzrule-dst-name` - 读取规则的夏令时缩写字段。
- `tzrule-dst-offset` - 读取规则的夏令时偏移字段。
- `tzrule-std-transition` - 读取规则退出夏令时的转移字段。
- `add-offset` - 把 time-delta 偏移量加到 UNIX 秒时间戳上。
- `transition-unix` - 计算某年 POSIX 转移发生的 UNIX 秒时刻。
- `in-offset` - 判断本地时刻相对某年 DST 转移区间的位置。
- `find-an-offset-rule` - 在 DST 规则年中定位本地时刻所属的年份与区间索引。
- `offset-rule` - 若本地时刻落在指定区间内则构造单元素 time-range 向量。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
