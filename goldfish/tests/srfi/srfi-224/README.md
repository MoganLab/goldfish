# (srfi srfi-224) 模块函数分类索引

`(srfi srfi-224)` 是 SRFI-224（fxmappings）的实现库，
提供以 fixnum 为键的有序映射 fxmapping：
兼具映射查询、区间操作与集合代数能力，
适合索引表、稠密整数键空间与范围裁剪等场景。
`(liii fxmapping)` 以 Goldfish 命名重导出本库全部函数。

## 常见用法示例

```scheme
(import (srfi srfi-224))

(define m (alist->fxmapping '((1 . "one") (2 . "two") (3 . "three"))))
(fxmapping-ref m 2)                 ; => "two"
(fxmapping-size m)                  ; => 3
(to-alist (fxmapping-filter (lambda (k v) (> k 1)) m)) ; 按键过滤
```

## 查看函数文档

本库叶子文档位于 tests/srfi 分组。使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```

## 函数分类索引

### 一、构造
- `fxmapping` - 由键值列表构造映射
- `alist->fxmapping` / `alist->fxmapping/combinator` - 由 alist 构造
- `fxmapping-unfold` - 按展开过程构造映射

### 二、判断与查询
- `fxmapping?` / `fxmapping-empty?` / `fxmapping-contains?` / `fxmapping-disjoint?`
- `fxmapping-ref` / `fxmapping-min` / `fxmapping-max` / `fxmapping-size`

### 三、增删改
- `fxmapping-adjoin` / `fxmapping-adjoin/combinator` / `fxmapping-set` / `fxmapping-adjust`
- `fxmapping-delete` / `fxmapping-delete-all` / `fxmapping-delete-min` / `fxmapping-delete-max`
- `fxmapping-pop-min` / `fxmapping-pop-max`

### 四、查找与遍历
- `fxmapping-find` / `fxmapping-count` / `fxmapping-any?` / `fxmapping-every?`
- `fxmapping-map` / `fxmapping-for-each` / `fxmapping-fold` / `fxmapping-fold-right`
- `fxmapping-map->list` / `fxmapping-filter` / `fxmapping-remove` / `fxmapping-partition`

### 五、转换
- `fxmapping->alist` / `fxmapping->decreasing-alist` / `fxmapping-keys` / `fxmapping-values`

### 六、比较
- `fxmapping=?` / `fxmapping<?` / `fxmapping>?` / `fxmapping<=?` / `fxmapping>=?`
- `fxsubmapping=`

### 七、集合运算
- `fxmapping-union` / `fxmapping-intersection` / `fxmapping-difference` / `fxmapping-xor`
- `fxmapping-union/combinator` / `fxmapping-intersection/combinator`

## 错误处理约定

- 键不是 fixnum 或参数类型不符时抛出 `type-error`
- 空映射上调用 `fxmapping-min` / `fxmapping-max` 等抛出值错误
