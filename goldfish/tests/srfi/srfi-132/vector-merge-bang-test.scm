;; vector-merge!
;; 合并两个已排序的向量（破坏性版本），当前实现尚未提供。
;;
;; 语法
;; ----
;; (vector-merge! cmp vec1 vec2)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; vec1 : vector
;; 第一个已排序向量。
;;
;; vec2 : vector
;; 第二个已排序向量。
;;
;; 返回值
;; ----
;; vector
;; 合并后的向量，与输入 vec1 是同一个对象。
;;
;; 错误处理
;; ----
;; ??? 当前实现尚未提供，调用时抛出未实现错误。

(import (liii check) (liii sort))

(check-set-mode! 'report-failed)

;; 当前实现尚未提供，调用即抛出未实现错误
(check-catch '??? (vector-merge! < #(1 3 5) #(2 4 6)))

(check-report)
