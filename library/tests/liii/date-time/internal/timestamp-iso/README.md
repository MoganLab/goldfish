# (liii date-time internal timestamp-iso) 模块函数分类索引

`(liii date-time internal timestamp-iso)` 实现时间戳与
ISO-8601 字符串之间的相互转换，是 (liii date-time timestamp)
的底层实现库。

## 常见用法示例

```scheme
(import (liii date-time timestamp)
        (liii date-time timezone))

(timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (utc-timezone)))
; => "2026-01-02T03:04:05Z"

(timestamp->utc-posix-time (iso-8601->timestamp "2026-01-02T03:04:05Z"))
; => 1767322 秒级的 UTC 时间点
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-iso "函数名"
```

## 函数分类索引

### 一、ISO-8601 转换
- `timestamp->iso-8601` - 将时间戳转换为 ISO-8601 字符串
- `iso-8601->timestamp` - 将 ISO-8601 字符串解析为时间戳

## 错误处理约定

- 参数不是时间戳或字符串时抛出 `type-error`
- 字符串不符合 ISO-8601 格式时抛出解析错误
