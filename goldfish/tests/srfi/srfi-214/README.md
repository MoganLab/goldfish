# (srfi srfi-214) 模块函数分类索引

`(srfi srfi-214)` 提供可变长向量（flexvector）：底层是可变长数组，支持随机访问，
并提供与普通向量、列表、字符串、生成器之间的互转，以及一整套原地修改操作。
适合需要频繁尾部操作、随机访问与分段处理的顺序容器场景。

## 常见用法示例

```scheme
(import (srfi srfi-214))

;; 示例1：创建一个可变长向量并访问元素
(define fv (flexvector 1 2 3))
(flexvector-length fv)
(flexvector-ref fv 1)

;; 示例2：向 flexvector 末尾追加元素
(flexvector->list (flexvector-add-back! (flexvector-copy fv) 4))

;; 示例3：带索引映射
(flexvector->list (flexvector-map/index (lambda (i x) (+ x i)) fv))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-214 "函数名"
```

## 函数分类索引

### 一、构造函数
- `make-flexvector` - 创建指定长度的可变长向量
- `flexvector` - 从多个元素直接创建可变长向量

### 二、谓词与访问器
- `flexvector?` - 判断对象是否为可变长向量
- `flexvector-empty?` - 判断是否为空
- `flexvector-ref` - 按索引访问元素
- `flexvector-front` - 获取首元素
- `flexvector-back` - 获取尾元素
- `flexvector-length` - 获取长度

### 三、原地修改
- `flexvector-set!` - 原地设置元素
- `flexvector-add!` - 在指定位置插入元素
- `flexvector-add-back!` - 在尾部追加元素
- `flexvector-add-front!` - 在头部插入元素
- `flexvector-remove!` - 删除指定位置元素
- `flexvector-remove-back!` - 删除尾元素
- `flexvector-remove-front!` - 删除首元素
- `flexvector-remove-range!` - 删除一段区间
- `flexvector-clear!` - 清空所有元素
- `flexvector-fill!` - 用同一值填充区间
- `flexvector-swap!` - 交换两个位置的元素
- `flexvector-reverse!` - 原地反转

### 四、复制与类型转换
- `flexvector-copy`、`flexvector-copy!` - 复制可变长向量
- `flexvector-reverse-copy`、`flexvector-reverse-copy!` - 反向复制
- `flexvector->vector`、`vector->flexvector` - 与普通向量互转
- `flexvector->list`、`list->flexvector` - 与列表互转
- `reverse-flexvector->list`、`reverse-list->flexvector` - 与列表反向互转
- `flexvector->string`、`string->flexvector` - 与字符串互转

### 五、遍历与搜索
- `flexvector-for-each`、`flexvector-for-each/index` - 遍历元素
- `flexvector-map`、`flexvector-map!`、`flexvector-map/index`、`flexvector-map/index!` - 映射
- `flexvector-fold`、`flexvector-fold-right` - 折叠
- `flexvector-filter`、`flexvector-filter!`、`flexvector-filter/index`、`flexvector-filter/index!` - 过滤
- `flexvector-append-map`、`flexvector-append-map/index` - 映射后拼接
- `flexvector-count`、`flexvector-cumulate` - 统计与累计
- `flexvector-index`、`flexvector-index-right`、`flexvector-skip`、`flexvector-skip-right` - 查找与跳过
- `flexvector-any`、`flexvector-every`、`flexvector-binary-search` - 判定与二分查找
- `flexvector-partition` - 拆分为满足与不满足两部分

### 六、拼接、比较与展开
- `flexvector-append`、`flexvector-concatenate`、`flexvector-append-subvectors`、`flexvector-append!` - 拼接
- `flexvector=?` - 比较两个可变长向量
- `flexvector-unfold`、`flexvector-unfold-right` - 展开构造
- `flexvector->generator`、`generator->flexvector` - 与生成器互转

### 七、复杂度速查

- O(1)：`flexvector?`、`flexvector-empty?`、`flexvector-length`、`flexvector-ref`、`flexvector-front`、`flexvector-back`、`flexvector-set!`、`flexvector-add-back!`、`flexvector-remove-back!`、`flexvector-clear!`、`flexvector-swap!`
- O(n)：`flexvector-copy`、`flexvector->list`、`list->flexvector`、`flexvector-map`、`flexvector-for-each`、`flexvector-fold`、`flexvector-filter`、`flexvector-index`、`flexvector-count`、`flexvector-any`、`flexvector-every`、`flexvector-add-front!`、`flexvector-remove!`、`flexvector-remove-front!`、`flexvector-add!`、`flexvector-fill!`、`flexvector-reverse!`、`flexvector-binary-search`（内部实现可能为线性）
