# (liii date-time internal date-week) 模块函数分类索引

`(liii date-time internal date-week)` 实现 ISO 星期、ISO 周数与 ISO 周年份的核心计算。

## 常见用法示例

```scheme
(import (liii date-time internal date-week))

(calc-date-iso-weekday (make-date 2021 1 4)) ; => 1
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/date-week "函数名"
```

## 函数索引

- `calc-date-iso-week` - 计算日期所在的 ISO 周数。
- `calc-date-iso-week-year` - 计算日期所属的 ISO 周年份。
- `calc-date-iso-weekday` - 计算日期的 ISO 星期（1=周一 到 7=周日）。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
