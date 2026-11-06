# (liii date-time internal date-arithmetic) 模块函数分类索引

`(liii date-time internal date-arithmetic)` 实现按年、月、周、日分量对日期做算术加法的内部算法。

## 常见用法示例

```scheme
(import (liii date-time internal date-arithmetic))

(date-add-components date 0 1 0 0) ; => date?
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/internal/date-arithmetic "函数名"
```

## 函数索引

- `date-add-components` - 按年、月、周、日分量对日期做算术加法，日期溢出时截断到月末。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
