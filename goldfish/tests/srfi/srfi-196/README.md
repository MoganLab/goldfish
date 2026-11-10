# (srfi srfi-196) 模块函数分类索引

SRFI-196 定义 `range`，一种惰性序列（lazy sequence）：元素按需计算，内存占用为 O(1)，
同时支持随机访问与切片操作。

与 `iota` 之类的立即生成列表不同：

- `iota` 立即生成完整列表，占用 O(n) 内存
- `range` 惰性求值，占用 O(1) 内存，并提供 `range-ref`、`subrange` 等随机访问接口

## 常见用法示例

```scheme
(import (srfi srfi-196))

;; 示例1：循环迭代
(range-for-each (lambda (x) (display x) (newline)) (numeric-range 0 5))

;; 示例2：循环求和
(range-fold + 0 (numeric-range 1 11))

;; 示例3：切片截取
(define r (numeric-range 0 100))
(range->list (range-take-right r 3))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-196 "函数名"
```

## 函数分类索引

### 一、构造函数
- `range` - 使用长度和索引器函数创建 range
- `numeric-range` - 创建数值范围 range
- `vector-range` - 从向量创建 range
- `string-range` - 从字符串创建 range
- `iota-range` - 创建类似 iota 的 range
- `range-append` - 连接多个 range

### 二、谓词函数
- `range?` - 判断是否为 range 对象
- `range=?` - 判断两个 range 是否相等
- `range-any` - 判断是否存在满足条件的元素
- `range-every` - 判断是否所有元素都满足条件

### 三、属性访问
- `range-length` - 获取 range 长度
- `range-first` - 获取第一个元素
- `range-last` - 获取最后一个元素

### 四、元素访问
- `range-ref` - 按索引获取元素
- `range-count` - 统计满足条件的元素数量

### 五、子范围操作
- `subrange` - 获取子 range
- `range-segment` - 将 range 分割成多个段
- `range-split-at` - 在指定位置分割 range
- `range-take` / `range-take-right` - 取前 n 个 / 后 n 个元素
- `range-drop` / `range-drop-right` - 跳过前 n 个 / 后 n 个元素
- `range-reverse` - 反转 range

### 六、遍历操作
- `range-map->list` / `range-map->vector` - 映射为列表 / 向量
- `range-for-each` - 遍历执行副作用
- `range-fold` / `range-fold-right` - 左折叠 / 右折叠

### 七、过滤操作
- `range-filter->list` / `range-filter->vector` - 过滤并转为列表 / 向量
- `range-remove->list` / `range-remove->vector` - 移除满足条件的元素并转为列表 / 向量

### 八、类型转换
- `vector->range` - 向量转为 range
- `range->list` / `range->vector` / `range->string` / `range->generator` - 转为列表 / 向量 / 字符串 / 生成器
