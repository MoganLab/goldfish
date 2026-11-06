# (liii date-time internal database-discover) 模块函数分类索引

`(liii date-time internal database-discover)` 枚举系统 zoneinfo 目录中可用的 IANA 时区名称。

## 常见用法示例

```scheme
(import (liii date-time internal database-discover))

(discover-zoneinfo-names) ; => ("UTC" ...)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/database-discover "函数名"
```

## 函数索引

- `discover-zoneinfo-names` - 枚举系统 zoneinfo 目录中可用的 IANA 时区名称。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
