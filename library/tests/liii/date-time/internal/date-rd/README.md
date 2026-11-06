# (liii date-time internal date-rd) 模块函数分类索引

`(liii date-time internal date-rd)` 实现日期与 raté die / 修正儒略日（MJD）之间的核心换算算法。

## 常见用法示例

```scheme
(import (liii date-time internal date-rd))

(calc-date->rata-die (make-date 1970 1 1)) ; => 719163
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/date-rd "函数名"
```

## 函数索引

- `unix-epoch-rd` - UNIX 纪元（1970-01-01）对应的 raté die 天数。
- `days-since-year-start` - 计算日期距离当年 1 月 1 日的天数（1-based）。
- `calc-date->rata-die` - 将日期换算为 raté die 天数。
- `calc-rata-die->date` - 将 raté die 天数换算为日期。
- `calc-date->mjd` - 将日期换算为修正儒略日（MJD）。
- `calc-mjd->date` - 将修正儒略日（MJD）换算为日期。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
