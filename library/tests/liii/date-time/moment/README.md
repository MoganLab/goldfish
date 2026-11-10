# (liii date-time moment) 模块函数分类索引

`(liii date-time moment)` 提供物理时刻 moment：
由日期与当日秒数（可含分数）构成，与时区无关，
适合时间戳内部表示、时刻比较与时间运算的中间值。

## 常见用法示例

```scheme
(import (liii date-time date)
        (liii date-time moment))

(define m (make-moment (make-date 2026 1 2) 3600))
(moment-date m)             ; => #<date 2026-01-02>
(moment-second-of-day m)    ; => 3600
(moment<? m (make-moment (make-date 2026 1 3) 0))  ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time/moment "函数名"
```

## 函数分类索引

### 一、构造与访问
- `make-moment` - 由日期与当日秒数构造物理时刻
- `moment?` - 判断是否为物理时刻
- `moment-date` - 获取时刻的日期部分
- `moment-second-of-day` - 获取时刻的当日秒数

### 二、比较
- `moment=?` - 时刻相等判断
- `moment<?` / `moment<=?` - 时刻先后判断
- `moment>?` / `moment>=?` - 时刻先后判断

## 错误处理约定

- 参数不是 moment 或类型不符时抛出 `type-error`
