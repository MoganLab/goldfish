# (liii comparator) 模块函数分类索引

`comparator` 提供「比较器」（comparator）抽象：一个比较器把「相等判定」「排序判定」
「类型判定」「哈希」四类操作打包在一起，便于在哈希表、排序、集合等场景中复用同一套语义。

`(liii comparator)` 重导出 `(srfi srfi-128)` 的全部接口。

## 常见用法示例

```scheme
(import (liii comparator))

;; 示例1：默认比较器
(define comp (make-default-comparator))
(<? comp 1 2) ; => #t

;; 示例2：自定义比较器
(define num (make-comparator number? = < number-hash))
(<? num 1 2) ; => #t

;; 示例3：列表比较器
(define lst (make-list-comparator num list? null? car cdr))
(=? lst '(1 2) '(1 2)) ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/comparator "函数名"
```

## 函数分类索引

以下函数全部由 `(srfi srfi-128)` 实现，对应单元测试位于 `tests/srfi/srfi-128/` 目录：

### 一、比较器构造
- `make-comparator` - 由类型判定、等价判定、排序判定、哈希四个过程创建比较器
- `make-default-comparator` - 创建默认比较器，按对象类型分派
- `make-eq-comparator` / `make-eqv-comparator` / `make-equal-comparator` - 分别以 `eq?`、`eqv?`、`equal?` 判定相等的比较器
- `make-pair-comparator` - 为二元组创建比较器
- `make-list-comparator` - 为列表创建比较器
- `make-vector-comparator` - 为向量创建比较器

### 二、比较器访问器
- `comparator?` - 判断对象是否为比较器
- `comparator-ordered?` - 判断比较器是否支持排序
- `comparator-hashable?` - 判断比较器是否支持哈希
- `comparator-type-test-predicate` - 取出类型判定过程
- `comparator-equality-predicate` - 取出相等判定过程
- `comparator-ordering-predicate` - 取出排序判定过程
- `comparator-hash-function` - 取出哈希过程

### 三、比较器操作
- `comparator-test-type` - 判断对象是否满足比较器的类型约束
- `comparator-check-type` - 检查对象是否满足类型约束，不满足时抛出 type-error
- `comparator-hash` - 使用比较器计算对象的哈希值

### 四、比较运算
- `=?` / `<?` / `>?` / `<=?` / `>=?` - 使用比较器比较两个或多个对象

### 五、哈希函数
- `default-hash` - 计算对象的默认哈希值
- `boolean-hash` / `char-hash` / `char-ci-hash` - 计算布尔值与字符的哈希值
- `string-hash` / `string-ci-hash` - 计算字符串的哈希值
- `symbol-hash` / `number-hash` - 计算符号与数值的哈希值
