# (liii range) 模块函数分类索引

`range` 是一种惰性序列（lazy sequence），元素按需计算，内存效率高。
与 `(liii list)` 的 `iota` 不同：

- `iota` 立即生成完整列表，占用 O(n) 内存
- `range` 惰性求值，占用 O(1) 内存，支持随机访问和切片操作

`(liii range)` 重导出 `(srfi srfi-196)` 的全部接口。

## 常见用法示例

```scheme
(import (liii range))

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
gf doc liii/range "函数名"
```

## 函数分类索引

以下函数全部由 `(srfi srfi-196)` 实现，对应单元测试位于 `tests/srfi/srfi-196/` 目录：

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
- `range-any` / `range-every` - 判断是否存在/所有元素满足条件

### 三、属性访问
- `range-length` / `range-first` / `range-last` - 获取长度 / 首元素 / 末元素

### 四、元素访问
- `range-ref` - 按索引获取元素
- `range-count` - 统计满足条件的元素数量

### 五、子范围操作
- `subrange` - 获取子 range
- `range-segment` / `range-split-at` - 分割 range
- `range-take` / `range-take-right` / `range-drop` / `range-drop-right` - 截取元素
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
