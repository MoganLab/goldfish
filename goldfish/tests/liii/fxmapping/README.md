# (liii fxmapping) 模块函数分类索引

`fxmapping` 是「函数映射」（function mapping）结构：把一组键映射到一组值，
并提供区间（interval）与子区间运算，常用于表示分段函数、稀疏表与区间数据。

`(liii fxmapping)` 重导出 `(srfi srfi-224)` 的全部接口。

## 常见用法示例

```scheme
(import (liii fxmapping))

;; 示例1：从关联列表构造
(define m (alist->fxmapping '((1 . 10) (2 . 20))))
(fxmapping-ref m 1) ; => 10

;; 示例2：判断键是否存在
(fxmapping-contains? m 2) ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/fxmapping "函数名"
```

## 函数分类索引

以下函数全部由 `(srfi srfi-224)` 实现，对应单元测试位于 `tests/srfi/srfi-224/` 目录：

### 一、构造函数
- `fxmapping` - 创建函数映射
- `fxmapping-unfold` - 通过 unfold 规则构造
- `alist->fxmapping` - 从关联列表构造
- `alist->fxmapping/combinator` - 返回把关联列表转换为函数映射的过程

### 二、谓词
- `fxmapping?` - 判断对象是否为函数映射
- `fxmapping-contains?` - 判断是否包含某个键
- `fxmapping-empty?` - 判断是否为空
- `fxmapping-disjoint?` - 判断两个映射是否不相交

### 三、访问器
- `fxmapping-ref` - 按键读取值
- `fxmapping-min` / `fxmapping-max` - 取最小 / 最大键

### 四、更新函数
- `fxmapping-adjoin` / `fxmapping-adjoin/combinator` - 加入键值对
- `fxmapping-set` / `fxmapping-adjust` - 覆盖 / 更新键对应的值
- `fxmapping-delete` / `fxmapping-delete-all` - 删除键
- `fxmapping-delete-min` / `fxmapping-delete-max` - 删除最小 / 最大键
- `fxmapping-pop-min` / `fxmapping-pop-max` - 取出并删除最小 / 最大键

### 五、整体操作
- `fxmapping-size` - 取键值对个数
- `fxmapping-find` / `fxmapping-count` - 查找 / 统计满足条件的键值对
- `fxmapping-any?` / `fxmapping-every?` - 判断是否存在 / 所有键值对满足条件
- `fxmapping-map` / `fxmapping-for-each` - 映射 / 遍历
- `fxmapping-fold` / `fxmapping-fold-right` - 左折叠 / 右折叠
- `fxmapping-map->list` - 映射为列表
- `fxmapping-filter` / `fxmapping-remove` - 过滤 / 移除
- `fxmapping-partition` - 拆分为两部分

### 六、类型转换
- `fxmapping->alist` / `fxmapping->decreasing-alist` - 转为关联列表
- `fxmapping-keys` / `fxmapping-values` - 取键 / 值列表

### 七、比较与集合运算
- `fxmapping=?` / `fxmapping<?` / `fxmapping>?` / `fxmapping<=?` / `fxmapping>=?` - 比较
- `fxmapping-union` / `fxmapping-intersection` / `fxmapping-difference` / `fxmapping-xor` - 集合运算
- `fxmapping-union/combinator` / `fxmapping-intersection/combinator` - 返回对应的组合过程

### 八、区间与子区间
- `fxmapping-open-interval` / `fxmapping-closed-interval` - 取开区间 / 闭区间
- `fxmapping-open-closed-interval` / `fxmapping-closed-open-interval` - 取半开区间
- `fxsubmapping=` / `fxsubmapping<` / `fxsubmapping<=` / `fxsubmapping>` / `fxsubmapping>=` - 子区间运算
- `fxmapping-split` - 按键拆分映射
