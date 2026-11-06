# (liii date-time internal format-util) 模块函数分类索引

`(liii date-time internal format-util)` 提供 date-time-format/parse 共享的格式化与解析辅助函数。

## 常见用法示例

```scheme
(import (liii date-time internal format-util))

(pad-zero 5 2) ; => "05"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/format-util "函数名"
```

## 函数索引

- `DEFAULT-FMT` - date-time-format/parse 缺省使用的格式化模板。
- `ABBR-WEEKDAY` - 星期名称缩写向量，索引 0-6 对应周日到周六。
- `LONG-WEEKDAY` - 星期全称向量，索引 0-6 对应周日到周六。
- `ABBR-MONTH` - 月份缩写向量，索引 1-12 对应一月到十二月，索引 0 为空串。
- `LONG-MONTH` - 月份全称向量，索引 1-12 对应一月到十二月，索引 0 为空串。
- `pad-zero` - 将数字转为字符串并用 '0' 左侧补齐到指定宽度。
- `string-ci-prefix?` - 判断 prefix 是否与 str 从 pos 起的片段大小写不敏感前缀匹配。
- `format-tz-offset` - 将 time-delta 偏移格式化为 +HHMM 或 +HH:MM 字符串。
- `expect-char` - 校验输入串 pos 处字符是否为期望字符，返回新位置。
- `read-digits` - 从 pos 起读取 10 进制数字，返回数值与新位置两个值。
- `read-fixed-digits` - 从 pos 起读取恰好 n 位数字，返回数值与新位置两个值。
- `read-microsecond` - 从 pos 起读取至多 6 位小数数字并按微秒补齐，返回值与新位置。
- `match-vector-name` - 在名称向量中查找输入串大小写不敏感的最长前缀匹配。
- `parse-tz-offset` - 解析 ISO-8601 时区偏移（Z、+HHMM、+HH:MM），返回增量与新位置。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
