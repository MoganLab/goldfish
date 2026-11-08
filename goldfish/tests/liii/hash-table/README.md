# (liii hash-table) 模块导览

`(liii hash-table)` 提供可变哈希表及其常见访问、更新和遍历接口，
适合缓存、索引表、配置项容器和高频键值查找场景。

本库完整重导出 `(srfi srfi-125)`，不额外定义任何函数，
因此各函数的文档与单元测试位于 [../../srfi/srfi-125/README.md](../../srfi/srfi-125/README.md)。

## 常用用法

```scheme
(import (liii hash-table))

;; 创建哈希表并读取已有键
(define ht (hash-table 'a 1 'b 2))
(hash-table-ref/default ht 'a 'missing) ; => 1

;; 判断是否包含某个键
(hash-table-contains? ht 'b)            ; => #t

;; 获取哈希表大小
(hash-table-size ht)                    ; => 2
```

## 查看函数文档

函数按 `(srfi srfi-125)` 归类，使用以下命令查看文档和用例：
```bash
gf doc srfi/srfi-125 "hash-table-ref"
```
或直接按函数名检索：
```bash
gf doc "hash-table-ref"
```
