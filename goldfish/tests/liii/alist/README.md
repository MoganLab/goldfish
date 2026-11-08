# (liii alist) 模块函数分类索引

`alist` 是 association list 的缩写，用 `(key . value)` 对组成普通列表。
与 `hash-table` 相比，它更轻量、可直接参与列表处理，适合小规模键值数据。

## 常见用法示例

```scheme
(import (liii alist))

;; 示例1：判断一个列表是否为 alist
(alist? '((name . "Goldfish") (age . 18)))

;; 示例2：按键查找，并在缺失时返回默认值
(alist-ref/default '((name . "Goldfish")) 'age 0)

;; 示例3：把向量转换成以索引为键的 alist
(vector->alist #(10 20 30))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/alist "函数名"
```

## 函数分类索引

### 一、谓词
- `alist?` - 判断一个列表是否由键值对组成

### 二、查找函数
- `alist-ref` - 按键查找值，未命中时抛出 `key-error`
- `alist-ref/default` - 按键查找值，未命中时返回默认值

### 三、转换函数
- `vector->alist` - 将向量转换为以索引为键的 alist

### 四、重导出的函数
- `alist-cons` - 由 `(liii list)` 实现，对应单元测试位于 `tests/liii/list/` 目录
