# (liii date-time tzdb) 模块函数分类索引

`(liii date-time tzdb)` 提供时区数据库管理：时区名称枚举、数据重载与时区集合访问。

## 常见用法示例

```scheme
(import (liii date-time tzdb))

(tz-timezones) ; => 全部可用 IANA 时区列表
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/tzdb "函数名"
```

## 函数索引


## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
