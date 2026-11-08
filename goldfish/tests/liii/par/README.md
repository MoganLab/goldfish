# (liii par) 模块函数分类索引

`par` 提供基于 `(liii go)` 线程池的并发列表与向量处理能力：
`par-map`、`par-filter`、`par-for-each` 及其向量版本会在后台 worker 线程中并发执行，
并在返回前等待全部任务完成。

## 常见用法示例

```scheme
(import (liii par))

;; 示例1：并发映射
(par-map (lambda (x) (* x 2)) '(1 2 3)) ; => (2 4 6)

;; 示例2：并发过滤
(par-filter even? '(1 2 3 4)) ; => (2 4)

;; 示例3：向量版本
(vector-par-map (lambda (x) (* x 2)) #(1 2 3)) ; => #(2 4 6)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/par "函数名"
```

## 函数分类索引

### 一、列表并发操作
- `par-map` - 并发映射列表元素，返回与输入等长且保序的结果列表
- `par-filter` - 并发过滤列表元素，返回满足判定条件的元素列表
- `par-for-each` - 并发遍历列表元素，对每个元素执行有副作用的过程

### 二、向量并发操作
- `vector-par-map` - 并发映射向量元素，返回与输入等长且保序的结果向量
- `vector-par-filter` - 并发过滤向量元素，返回满足判定条件的元素向量
- `vector-par-for-each` - 并发遍历向量元素，对每个元素执行有副作用的过程
