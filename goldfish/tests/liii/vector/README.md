# (liii vector) 模块函数分类索引

`vector` 提供向量切片、查找、过滤以及类型化向量（int-vector、float-vector、complex-vector）与子向量（subvector）操作。

## 常见用法示例

```scheme
(import (liii vector))

;; 示例1：截取向量前半段
(vector->list (vector-take #(1 2 3 4) 2))

;; 示例2：判断向量中是否包含某个元素
(vector-contains? #(1 2 3) 2)

;; 示例3：跳过前一个元素并转回列表
(vector->list (vector-drop #(1 2 3 4) 1))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/vector "函数名"
```

## 函数分类索引

### 一、切片与过滤
- `vector-take` - 取前 n 个元素
- `vector-drop` - 跳过前 n 个元素
- `vector-take-right` - 取后 n 个元素
- `vector-drop-right` - 跳过后 n 个元素
- `vector-filter` - 过滤向量元素
- `vector-contains?` - 判断向量是否包含元素

### 二、整数向量 (int-vector)
- `int-vector` - 构造整数向量
- `int-vector?` - 判断是否为整数向量
- `make-int-vector` - 创建指定长度的整数向量
- `int-vector-ref` - 读取整数向量元素
- `int-vector-set!` - 原地设置整数向量元素

### 三、浮点向量 (float-vector)
- `float-vector` - 构造浮点向量
- `float-vector?` - 判断是否为浮点向量
- `make-float-vector` - 创建指定长度的浮点向量
- `float-vector-ref` - 读取浮点向量元素
- `float-vector-set!` - 原地设置浮点向量元素

### 四、复数向量 (complex-vector)
- `complex-vector` - 构造复数向量
- `complex-vector?` - 判断是否为复数向量
- `make-complex-vector` - 创建指定长度的复数向量
- `complex-vector-ref` - 读取复数向量元素
- `complex-vector-set!` - 原地设置复数向量元素

### 五、子向量 (subvector)
- `subvector` - 创建子向量
- `subvector?` - 判断是否为子向量
- `subvector-position` - 获取子向量在原向量中的起始偏移
- `subvector-vector` - 获取子向量关联的原向量
