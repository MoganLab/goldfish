# (liii flexvector) 模块函数分类索引

`flexvector` 是可变长向量，兼具向量随机访问能力与动态扩缩容能力，
适合需要频繁追加、删除、映射和分段处理的顺序容器场景。
`(liii flexvector)` 是 `(srfi srfi-214)` 的重导出，完整提供 SRFI-214 的可变长向量接口。

## 常见用法示例

```scheme
(import (liii flexvector))

;; 示例1：创建一个可变长向量并访问元素
(define fv (flexvector 1 2 3))
(flexvector-length fv)
(flexvector-ref fv 1)

;; 示例2：向 flexvector 末尾追加元素
(flexvector->list (flexvector-add-back! (flexvector-copy fv) 4))

;; 示例3：拼接两个 flexvector
(flexvector->list (flexvector-append fv (flexvector 4 5)))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/flexvector "函数名"
```

## 函数分类索引

`(liii flexvector)` 不新增实现，全部函数由 `(srfi srfi-214)` 提供，
对应单元测试位于 `tests/srfi/srfi-214/` 目录：

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
