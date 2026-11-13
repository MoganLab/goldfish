# (liii set) 模块函数分类索引

`set` 提供去重后的集合结构，适合成员测试、去重、集合代数和无序数据聚合。
`(liii set)` 基于 `(srfi srfi-113)`，使用默认比较器简化了 `set` 和 `list->set` 的构造，并重导出了 SRFI-113 的集合操作函数。

## 常见用法示例

```scheme
(import (liii set))

;; 示例1：构造一个普通集合
(define s (set 1 2 3))
(set-contains? s 2)

;; 示例2：读取集合大小
(set-size s)

;; 示例3：计算两个集合的并集
(set->list (set-union s (set 3 4)))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/set "函数名"
```

## 函数分类索引

### 一、构造函数
- `set` - 使用默认比较器从多个元素直接构造集合
- `list->set` - 使用默认比较器从列表构造集合
- `make-set-with-comparator` - 使用指定比较器从多个元素构造集合
- `list->set-with-comparator` - 使用指定比较器从列表构造集合

### 二、重导出的 SRFI-113 集合函数

以下函数由 `(srfi srfi-113)` 实现，对应单元测试位于 `tests/srfi/srfi-113/` 目录：

- 构造函数与转换：`set-unfold`、`list->set!`、`set-copy`、`set->list`、`bag->set`、`set->bag`、`set->bag!`
- 谓词与访问器：`set?`、`set-contains?`、`set-empty?`、`set-disjoint?`、`set-element-comparator`、`set-member`、`set-comparator`
- 统计与遍历：`set-size`、`set-find`、`set-count`、`set-any?`、`set-every?`、`set-map`、`set-for-each`、`set-fold`
- 过滤与划分：`set-filter`、`set-filter!`、`set-remove`、`set-remove!`、`set-partition`、`set-partition!`
- 集合比较：`set=?`、`set<?`、`set>?`、`set<=?`、`set>=?`
- 集合运算：`set-union`、`set-intersection`、`set-difference`、`set-xor` 及其破坏性版本（`set-union!`、`set-intersection!`、`set-difference!`、`set-xor!`）
- 更新函数：`set-adjoin`、`set-adjoin!`、`set-replace`、`set-replace!`、`set-delete`、`set-delete!`、`set-delete-all`、`set-delete-all!`、`set-search!`
