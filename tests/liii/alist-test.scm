;; (liii alist) 模块函数分类索引
;;
;; alist 是 association list 的缩写，用 `(key . value)` 对组成普通列表。
;; 与 hash-table 相比，它更轻量、可直接参与列表处理，适合小规模键值数据。


;; ==== 常见用法示例 ====
(import (liii alist))


;; 示例1：判断一个列表是否为 alist
(alist? '((name . "Goldfish") (age . 18))
) ;alist?


;; 示例2：按键查找，并在缺失时返回默认值
(alist-ref/default '((name . "Goldfish"))
  'age
  0
) ;alist-ref/default


;; 示例3：把向量转换成以索引为键的 alist
(vector->alist #(10 20 30))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/alist "alist?"
;;   bin/gf doc liii/alist "alist-ref"


;; ==== 函数分类索引 ====


;; 一、谓词与构造
;; 用于创建和判断 alist 的函数
;;   alist?             - 判断一个列表是否由键值对组成
;;   alist-cons         - 向 alist 前端插入一个键值对


;; 二、查找函数
;; 用于按键读取 alist 中值的函数
;;   alist-ref          - 按键查找值，未命中时通常抛错
;;   alist-ref/default  - 按键查找值，未命中时返回默认值


;; 三、转换函数
;; 用于把其他结构转成 alist 的函数
;;   vector->alist      - 将向量转换为以索引为键的 alist
