# (srfi srfi-158) 模块函数分类索引

generator（生成器）是一个无参过程，每次调用返回下一个元素，耗尽时返回 eof-object；
accumulator（累加器）是一个单参过程，接收元素进行累计，接收 eof-object 时返回最终计算结果。
生成器可用于惰性求值、无限序列、管道式数据处理与流式转换。

## 常见用法示例

```scheme
(import (srfi srfi-158))

;; 示例1：构造生成器并遍历
(define g (generator 1 2 3))
(g) ; => 1
(g) ; => 2
(g) ; => 3
(g) ; => eof-object

;; 示例2：生成器转换为列表
(generator->list (make-iota-generator 4 10 2)) ; => '(10 12 14 16)

;; 示例3：生成器映射与过滤
(generator->list (gtake (gfilter odd? (make-range-generator 0 100)) 3)) ; => '(1 3 5)

;; 示例4：累加器收集
(define acc (list-accumulator))
(acc 'a)
(acc 'b)
(acc (eof-object)) ; => '(a b)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-158 "函数名"
```

## 函数分类索引

### 一、生成器构造函数
- `generator` - 创建依次产出指定参数的生成器
- `circular-generator` - 创建无限循环产出参数序列的生成器
- `make-iota-generator` - 创建等差数列生成器
- `make-range-generator` - 创建数值区间生成器
- `make-coroutine-generator` - 创建协程生成器
- `make-for-each-generator` - 将 for-each 类过程转换为生成器
- `make-unfold-generator` - 创建展开算法生成器

### 二、转换为生成器
- `list->generator` - 列表转生成器
- `vector->generator` - 向量转生成器
- `reverse-vector->generator` - 逆序向量转生成器
- `string->generator` - 字符串转生成器
- `bytevector->generator` - 字节向量转生成器

### 三、从生成器转换
- `generator->list` - 生成器转列表
- `generator->reverse-list` - 生成器转逆序列表
- `generator->vector` - 生成器转向量
- `generator->vector!` - 生成器写入现有向量
- `generator->string` - 生成器转字符串
- `generator-map->list` - 映射生成器并转列表

### 四、生成器运算
- `gcons*` - 在生成器前追加元素
- `gappend` - 拼接多个生成器
- `gflatten` - 展平嵌套列表
- `ggroup` - 分组
- `gmerge` - 归并
- `gmap` - 映射
- `gcombine` - 多生成器组合
- `gfilter` - 过滤
- `gremove` - 移除
- `gstate-filter` - 带状态的过滤
- `gtake`、`gdrop` - 取前/丢弃前若干元素
- `gtake-while`、`gdrop-while` - 按谓词取前/丢弃前若干元素
- `gdelete` - 删除指定元素
- `gdelete-neighbor-dups` - 删除相邻重复元素
- `gindex` - 按索引生成器取元素
- `gselect` - 按布尔生成器筛选元素

### 五、归约与谓词
- `generator-fold` - 折叠
- `generator-for-each` - 遍历
- `generator-find` - 查找
- `generator-count` - 统计
- `generator-any` - 判断是否存在
- `generator-every` - 判断是否全部满足
- `generator-unfold` - 展开

### 六、累加器
- `make-accumulator` - 创建自定义累加器
- `list-accumulator`、`reverse-list-accumulator` - 列表累加器
- `vector-accumulator`、`reverse-vector-accumulator`、`vector-accumulator!` - 向量累加器
- `string-accumulator` - 字符串累加器
- `bytevector-accumulator`、`bytevector-accumulator!` - 字节向量累加器
- `sum-accumulator`、`product-accumulator` - 求和/求积累加器
