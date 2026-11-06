# (liii date-time gregorian) 模块函数分类索引

`(liii date-time gregorian)` 提供公历辅助函数：闰年判断、每月天数与第 N 个星期几计算。

## 常见用法示例

```scheme
(import (liii date-time gregorian))

(leap-year? 2000) ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/gregorian "函数名"
```

## 函数索引

- `leap-year?`
- `days-in-month` - 获取公历某年某月的天数。
- `day-name->weekday` - 将星期符号转换为 ISO 星期序号（1-7，周一为 1，周日为 7）。
- `first-in-month` - 获取某年某月中第一个指定的星期几对应的日期号（day of month）。
- `last-in-month` - 获取某年某月中最后一个指定的星期几对应的日期号（day of month）。
- `nth-in-month` - 获取某年某月中第 n 个指定的星期几对应的日期号（day of month）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
