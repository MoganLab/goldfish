# (liii date-time internal tzif) 模块函数分类索引

`(liii date-time internal tzif)` 解析 IANA TZif 二进制文件为本地时间类型转移记录向量。

## 常见用法示例

```scheme
(import (liii date-time internal tzif))

(read-tzif-file "/usr/share/zoneinfo/UTC")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/tzif "函数名"
```

## 函数索引

- `tzif-transition` - 构造一条 TZif 本地时间类型转移记录。
- `tzif-transition?` - 判断对象是否为 TZif 转移记录。
- `tzif-transition-time` - 读取 tzif-transition 记录的时刻字段。
- `tzif-transition-offset` - 读取 tzif-transition 记录的 转移后的 字段。
- `tzif-transition-dst?` - 读取 tzif-transition 记录的 转移后是否处于夏令时 字段。
- `tzif-transition-designation` - 读取 tzif-transition 记录的 转移后的时区缩写 字段。
- `tzif-transition-standard?` - 读取 tzif-transition 记录的 转移类型是否为本地标准时间 字段。
- `tzif-transition-universal-time?` - 读取 tzif-transition 记录的 转移时刻是否按 字段。
- `read-tzif-file` - 读取 TZif 文件并解析为时区转移数据。
- `read-tzif-bytevector` - 解析 TZif 字节向量为时区转移数据。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
