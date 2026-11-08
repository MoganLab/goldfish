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
