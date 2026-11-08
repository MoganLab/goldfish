# (srfi srfi-217) 模块函数分类索引

SRFI-217 定义 `iset`（整数集合），一种针对整数域优化的集合结构。
它以压缩二进制基数树（Patricia trie）实现，适合稠密整数集合、区间裁剪与范围合并，
并提供区间（interval）与子区间（subinterval）运算。

## 常见用法示例

```scheme
(import (srfi srfi-217))

;; 示例1：从整数列表构造一个 iset
(define s (list->iset '(1 2 3 5 8)))

;; 示例2：判断某个整数是否为成员
(iset-contains? s 5)

;; 示例3：直接创建一个整数区间集合
(make-range-iset 10 15)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-217 "函数名"
```

## 函数分类索引

### 一、构造函数
- `iset` - 从多个整数直接构造 iset
- `iset-unfold` - 通过 unfold 规则构造 iset
- `make-range-iset` - 按整数区间构造 iset
- `iset-copy` - 复制一个 iset
- `iset->list` - 转为整数列表
- `list->iset` - 从列表构造 iset
- `list->iset!` - 原地式从列表构造 iset

### 二、谓词与访问
- `iset?` - 判断对象是否为 iset
- `iset-contains?` - 判断是否包含某个整数
- `iset-empty?` - 判断 iset 是否为空
- `iset-disjoint?` - 判断两个 iset 是否不相交
- `iset-member` - 查找成员值
- `iset-min` - 获取最小值
- `iset-max` - 获取最大值

### 三、更新函数
- `iset-adjoin` / `iset-adjoin!` - 加入整数
- `iset-delete` / `iset-delete!` - 删除整数
- `iset-delete-all` / `iset-delete-all!` - 删除多个整数
- `iset-search` / `iset-search!` - 搜索并返回（或就地更新）集合
- `iset-delete-min` / `iset-delete-min!` - 删除最小值
- `iset-delete-max` / `iset-delete-max!` - 删除最大值

### 四、统计与遍历
- `iset-size` - 获取元素个数
- `iset-find` - 查找满足条件的值
- `iset-count` - 统计满足条件的值个数
- `iset-any?` / `iset-every?` - 判断是否存在/全部满足条件的值
- `iset-map` - 映射为其他序列
- `iset-for-each` - 遍历所有值
- `iset-fold` / `iset-fold-right` - 左折叠 / 右折叠
- `iset-filter` / `iset-filter!` - 过滤成员
- `iset-remove` / `iset-remove!` - 移除满足条件的成员
- `iset-partition` / `iset-partition!` - 拆分为两部分

### 五、集合与区间运算
- `iset=?` / `iset<?` / `iset>?` / `iset<=?` / `iset>=?` - 集合比较
- `iset-union` / `iset-union!` - 并集
- `iset-intersection` / `iset-intersection!` - 交集
- `iset-difference` / `iset-difference!` - 差集
- `iset-xor` / `iset-xor!` - 对称差
- `iset-open-interval` / `iset-closed-interval` - 取开区间 / 闭区间
- `iset-open-closed-interval` / `iset-closed-open-interval` - 取半开区间
- `isubset=` / `isubset<` / `isubset<=` / `isubset>` / `isubset>=` - 子区间运算
