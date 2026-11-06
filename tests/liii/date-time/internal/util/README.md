# (liii date-time internal util) 模块函数分类索引

`(liii date-time internal util)` 提供 date-time 各内部库共享的通用小工具函数。

## 常见用法示例

```scheme
(import (liii date-time internal util))

(left-pad "5" #\0 2) ; => "05"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/util "函数名"
```

## 函数索引

- `left-pad` - 在字符串左侧填充指定字符至目标长度。
- `floor/` - 对两个整数做向下取整除法，返回商与余数两个值。
- `truncate/` - 对两个整数做截断除法，返回商与余数两个值。
- `unix-timestamp->ntp-timestamp` - 将 UNIX 秒时间戳换算为 NTP 秒时间戳。
- `ntp-timestamp->unix-timestamp` - 将 NTP 秒时间戳换算为 UNIX 秒时间戳。
- `make-chain-comparator` - 构造支持链式比较的比较器工厂。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
