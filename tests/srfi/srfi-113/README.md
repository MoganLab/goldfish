# (srfi srfi-113) 模块函数分类索引

`(srfi srfi-113)` 提供 set 与 bag 两类多重集合结构：set 去除重复元素，bag 保留元素的出现次数。
集合运算（并集、交集、差集、对称差）以多重集语义实现，所有构造函数都显式接受比较器。

## 常见用法示例

```scheme
(import (srfi srfi-113) (srfi srfi-128))

(define comp (make-default-comparator))

;; 示例1：构造一个普通集合
(define s (set comp 1 2 3))
(set-contains? s 2)

;; 示例2：构造一个保留重复次数的多重集
(define b (bag comp 1 2 2))
(bag-count (lambda (x) (= x 2)) b)

;; 示例3：计算两个集合的并集
(set->list (set-union s (set comp 3 4)))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-113 "函数名"
```

## 函数分类索引

### 一、构造函数
- `set` - 使用指定比较器从多个元素直接构造集合
- `list->set` - 使用指定比较器从列表构造集合
- `list->set!` - 就地向集合追加列表中的元素
- `set-unfold` - 通过 unfold 规则生成集合
- `bag` - 使用指定比较器从多个元素直接构造多重集
- `list->bag` - 使用指定比较器从列表构造多重集
- `list->bag!` - 就地向多重集追加列表中的元素
- `bag-unfold` - 通过 unfold 规则生成多重集

### 二、谓词与访问器
- `set?`、`bag?` - 判断对象是否为集合或多重集
- `set-contains?`、`bag-contains?` - 判断是否包含指定元素
- `set-empty?`、`bag-empty?` - 判断是否为空
- `set-disjoint?`、`bag-disjoint?` - 判断两个集合是否不相交
- `set-element-comparator`、`bag-comparator` - 获取比较器
- `set-size`、`bag-size` - 获取元素总数
- `set->list`、`bag->list` - 转换为列表
- `set-copy`、`bag-copy` - 复制集合
- `set-member`、`bag-member` - 查找成员
- `set-find`、`bag-find` - 查找满足条件的元素
- `set-count`、`bag-count` - 统计满足条件的元素个数
- `set-any?`、`bag-any?` - 判断是否存在满足条件的元素
- `set-every?`、`bag-every?` - 判断是否所有元素都满足条件

### 三、集合运算
- `set=?`、`set<?`、`set>?`、`set<=?`、`set>=?` - 比较集合
- `bag=?`、`bag<?`、`bag>?`、`bag<=?`、`bag>=?` - 比较多重集
- `set-union`、`bag-union` 及其破坏性版本
- `set-intersection`、`bag-intersection` 及其破坏性版本
- `set-difference`、`bag-difference` 及其破坏性版本
- `set-xor`、`bag-xor` 及其破坏性版本
- `set-partition` 及其破坏性版本

### 四、更新函数
- `set-adjoin`、`bag-adjoin` 及其破坏性版本
- `set-replace`、`bag-replace` 及其破坏性版本
- `set-delete`、`bag-delete` 及其破坏性版本
- `set-delete-all`、`bag-delete-all` 及其破坏性版本
- `set-search!`、`bag-search!` - 通过搜索过程就地更新元素

### 五、遍历与过滤
- `set-map`、`set-for-each`、`set-fold` 及其破坏性版本
- `set-filter`、`set-remove`、`set-partition` 及其破坏性版本
