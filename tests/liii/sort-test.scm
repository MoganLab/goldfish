;; (liii sort) 模块函数分类索引
;;
;; sort 提供列表和向量的排序、稳定排序、归并和“是否已排序”判断。
;; 适合需要显式传入比较函数的通用排序场景。

;; ==== 常见用法示例 ====
(import (liii sort))

;; 示例1：对列表进行排序
(list-sort < '(3 1 4 2)) ; => (1 2 3 4)

;; 示例2：对向量进行排序
(vector-sort < #(3 1 4 2)) ; => #(1 2 3 4)

;; 示例3：判断一个向量是否已排序
(vector-sorted? < #(1 2 3 4)) ; => #t

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/sort "list-sort"
;;   bin/gf doc liii/sort "vector-sorted?"

;; ==== 函数分类索引 ====

;; 一、已排序判断
;; 用于判断序列是否已经有序的函数
;;   list-sorted?          - 判断列表是否已排序
;;   vector-sorted?        - 判断向量是否已排序

;; 二、归并函数
;; 用于合并两个有序序列的函数
;;   list-merge            - 归并两个有序列表
;;   list-merge!           - 原地归并两个有序列表
;;   vector-merge          - 归并两个有序向量
;;   vector-merge!         - 原地归并两个有序向量

;; 三、排序函数
;; 用于对列表和向量执行常规排序的函数
;;   list-sort             - 排序列表
;;   list-sort!            - 原地排序列表
;;   vector-sort           - 排序向量
;;   vector-sort!          - 原地排序向量

;; 四、稳定排序函数
;; 用于保持相等元素相对次序的排序函数
;;   list-stable-sort      - 稳定排序列表
;;   list-stable-sort!     - 原地稳定排序列表
;;   vector-stable-sort    - 稳定排序向量
;;   vector-stable-sort!   - 原地稳定排序向量
