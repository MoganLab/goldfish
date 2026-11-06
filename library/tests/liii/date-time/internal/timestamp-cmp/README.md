# (liii date-time internal timestamp-cmp) 模块函数分类索引

`(liii date-time internal timestamp-cmp)` 提供基于物理时刻的时间戳比较器工厂。

## 常见用法示例

```scheme
(import (liii date-time internal timestamp-cmp))

(make-timestamp-comparator (lambda (r) (< r 0)))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/timestamp-cmp "函数名"
```

## 函数索引

- `make-timestamp-comparator` - 基于物理时刻构造时间戳链式比较器。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
