# (srfi srfi-165) 模块函数分类索引

SRFI-165 提供「计算」（computation）抽象：一个计算对象接收环境并产出结果，
计算之间可以组合（绑定、顺序执行、分支），从而表达带环境的数据处理流程。

计算类型（computation type）由 `define-computation-type` 定义：
它声明一组静态字段（计算类型的固定槽位）以及环境构造过程与运行过程。

## 常见用法示例

```scheme
(import (srfi srfi-165))

;; 示例1：定义计算类型
(define-computation-type make-env run
  (width 78)
)

;; 示例2：纯计算
(run (computation-pure 42)) ; => 42

;; 示例3：绑定与读取环境变量
(define v (make-computation-environment-variable 'x 100 #f))
(run (computation-bind (computation-ask)
         (lambda (e) (computation-pure (computation-environment-ref e v)))
       )) ; => 100
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-165 "函数名"
```

## 函数分类索引

### 一、计算环境变量
- `make-computation-environment-variable` - 创建计算环境变量
- `default-computation` - 默认计算变量
- `variable-comparator` - 环境变量的比较器

### 二、计算环境
- `computation-environment-ref` - 读取环境中的变量值
- `computation-environment-update` - 在环境副本上绑定变量，返回新环境
- `computation-environment-update!` - 就地修改环境中的变量绑定
- `computation-environment-copy` - 复制计算环境

### 三、计算构造
- `make-computation` - 创建计算对象
- `computation-pure` - 创建返回常量值的计算
- `computation-ask` - 创建请求当前计算环境的计算
- `computation-local` - 在局部环境中执行计算

### 四、计算组合
- `computation-each` / `computation-each-in-list` - 依次执行多个计算
- `computation-bind` - 把计算结果传给后续过程
- `computation-sequence` - 依次执行计算并收集结果
- `computation-forked` - 在环境副本上依次执行计算
- `computation-bind/forked` - 在环境副本上绑定计算结果

### 五、语法糖
- `computation-fn` - 创建读取环境变量并返回计算的宏
- `computation-with` - 在计算中绑定环境变量并执行后续计算
- `computation-with!` - 在计算中就地绑定环境变量

### 六、计算类型
- `define-computation-type` - 定义计算类型：环境构造过程、运行过程与静态字段
