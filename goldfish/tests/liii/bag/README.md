# (liii bag) 模块函数分类索引

`bag` 是一种多重集（multiset），与 `set` 不同，它会保留元素出现的次数。
适合频次统计、去重前聚合，以及“允许重复值”的集合运算。
`(liii bag)` 基于 `(srfi srfi-113)`，使用默认比较器简化了 `bag` 和 `list->bag` 的构造，并重导出了 SRFI-113 的多重集操作函数。

## 常见用法示例

```scheme
(import (liii bag))

;; 示例1：创建一个会保留重复元素的 bag
(define scores (bag 1 2 2 3))
(bag-size scores)

;; 示例2：查询某个元素是否存在，以及它的代表值
(bag-member scores 2 #f)

;; 示例3：向 bag 中加入新元素
(bag->list (bag-adjoin scores 5))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/bag "函数名"
```

## 函数分类索引

### 一、构造函数
- `bag` - 使用默认比较器从多个元素直接构造多重集
- `list->bag` - 使用默认比较器从列表构造多重集

### 二、重导出的 SRFI-113 多重集函数

以下函数由 `(srfi srfi-113)` 实现，对应单元测试位于 `tests/srfi/srfi-113/` 目录：

- 构造函数：`bag-unfold`、`list->bag!`、`bag-copy`、`bag-comparator`、`bag->list`
- 谓词函数：`bag?`、`bag-contains?`、`bag-empty?`、`bag-disjoint?`
- 统计与比较：`bag-size`、`bag-find`、`bag-count`、`bag-any?`、`bag-every?`、`bag=?`、`bag<?`、`bag>?`、`bag<=?`、`bag>=?`
- 集合运算：`bag-union`、`bag-intersection`、`bag-difference`、`bag-xor` 及其破坏性版本
- 更新函数：`bag-adjoin`、`bag-replace`、`bag-delete`、`bag-delete-all`、`bag-search!` 及其破坏性版本
