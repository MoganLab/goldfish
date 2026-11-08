# (liii sort) 模块函数分类索引

`sort` 提供列表与向量的排序、归并与有序性判定能力，
所有函数都接受自定义比较函数，并同时提供非破坏性与破坏性两个版本。
`(liii sort)` 是 `(srfi srfi-132)` 的重导出，完整提供 SRFI-132 的排序接口。

## 常见用法示例

```scheme
(import (liii sort))

;; 示例1：对列表排序
(list-sort < '(3 1 4 1 5 9 2 6)) ; => '(1 1 2 3 4 5 6 9)

;; 示例2：判断列表是否有序
(list-sorted? < '(1 2 3)) ; => #t

;; 示例3：对向量原地排序
(vector-sort! < (vector 3 1 2)) ; => #(1 2 3)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/sort "函数名"
```

## 函数分类索引

`(liii sort)` 不新增实现，全部函数由 `(srfi srfi-132)` 提供，
对应单元测试位于 `tests/srfi/srfi-132/` 目录：

### 一、有序性判定
- `list-sorted?` - 判断列表是否按给定比较函数有序
- `vector-sorted?` - 判断向量是否按给定比较函数有序

### 二、排序
- `list-sort` - 对列表排序，返回新列表
- `list-sort!` - 对列表原地排序
- `list-stable-sort` - 对列表稳定排序，返回新列表
- `list-stable-sort!` - 对列表原地稳定排序
- `vector-sort` - 对向量排序，返回新向量
- `vector-sort!` - 对向量原地排序
- `vector-stable-sort` - 对向量稳定排序，返回新向量
- `vector-stable-sort!` - 对向量原地稳定排序（当前实现尚未提供）

### 三、归并
- `list-merge` - 合并两个已排序列表，返回新列表
- `list-merge!` - 合并两个已排序列表，原地写入第一个列表
- `vector-merge` - 合并两个已排序向量，返回新向量
- `vector-merge!` - 合并两个已排序向量，原地写入第一个向量（当前实现尚未提供）
