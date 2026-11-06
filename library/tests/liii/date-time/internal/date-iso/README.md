# (liii date-time internal date-iso) 模块函数分类索引

`(liii date-time internal date-iso)` 实现日期与 ISO-8601 字符串之间的相互转换。

## 常见用法示例

```scheme
(import (liii date-time internal date-iso))

(format-date-iso-8601 (make-date 2021 1 2)) ; => "2021-01-02"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/date-iso "函数名"
```

## 函数索引

- `format-date-iso-8601` - 将日期格式化为 YYYY-MM-DD 字符串。
- `parse-date-iso-8601` - 解析 YYYY-MM-DD 字符串为日期对象。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
