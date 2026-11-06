# (liii json schema format-datetime) 模块函数分类索引

`(liii json schema format-datetime)` 实现 JSON Schema `format` 关键字中
`date`、`time`、`date-time` 与 `duration` 四个格式的检查。

日期时间遵循 RFC 3339，时长遵循 ISO 8601。

## 常见用法示例

```scheme
(import (liii json schema format-datetime))

(date? "2020-02-29")            ; => #t
(time? "12:00:00Z")             ; => #t
(date-time? "2020-01-01T12:00:00Z")   ; => #t
(duration? "P1Y2M3DT4H5M6S")    ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-datetime "函数名"
```

## 函数分类索引

### 一、日期
- `date?` - 判断字符串是否为 RFC 3339 完整日期

### 二、时间
- `time?` - 判断字符串是否为 RFC 3339 完整时间

### 三、日期时间
- `date-time?` - 判断字符串是否为 RFC 3339 日期时间

### 四、时长
- `duration?` - 判断字符串是否为 ISO 8601 时长

## 错误处理约定

- 四个谓词都返回布尔值，不会抛出异常
- `time?` 与 `date-time?` 要求时间部分带时区
- `duration?` 要求以大写 `P` 开头
