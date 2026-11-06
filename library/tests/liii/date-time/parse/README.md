# (liii date-time parse) 模块函数分类索引

`(liii date-time parse)` 按格式串将字符串解析为时间戳。

## 常见用法示例

```scheme
(import (liii date-time parse))

(date-time-parse "2026-01-02 03:04:05" "%Y-%m-%d %H:%M:%S")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/parse "函数名"
```

## 函数索引

- `date-time-parse` - 将格式化的字符串解析为 timestamp 对象，说明符与 Python datetime.strptime 对齐。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
