# (liii date-time format) 模块函数分类索引

`(liii date-time format)` 按格式串将时间戳格式化为字符串。

## 常见用法示例

```scheme
(import (liii date-time format))

(date-time-format ts "%Y-%m-%d") ; => "2026-01-02"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/format "函数名"
```

## 函数索引

- `date-time-format` - 将日期时间对象按格式字符串格式化为字符串，格式说明符与 Python datetime.strftime 对齐。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
