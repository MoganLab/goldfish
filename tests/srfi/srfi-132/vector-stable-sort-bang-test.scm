;; vector-stable-sort!
;; 对向量进行原地稳定排序，当前实现尚未提供。
;;
;; 语法
;; ----
;; (vector-stable-sort! less? vec)
;; (vector-stable-sort! less? vec start)
;; (vector-stable-sort! less? vec start end)
;;
;; 参数
;; ----
;; less? : procedure
;; 比较函数，接受两个元素，当第一个元素应排在前面时返回 #t。
;;
;; vec : vector
;; 要排序的向量。
;;
;; start : integer
;; 排序区间的起始下标（包含），默认为 0。
;;
;; end : integer
;; 排序区间的结束下标（不包含），默认为 (vector-length vec)。
;;
;; 返回值
;; ----
;; vector
;; 排序后的向量，与输入 vec 是同一个对象。
;;
;; 错误处理
;; ----
;; ??? 当前实现尚未提供，调用时抛出未实现错误。

(import (liii check) (liii sort))

(check-set-mode! 'report-failed)

;; 当前实现尚未提供，调用即抛出未实现错误
(check-catch '??? (vector-stable-sort! < #(3 1 2)))

(check-report)
