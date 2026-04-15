(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-length
;; 获取向量的长度。
;;
;; 语法
;; ----
;; (vector-length vec)
;;
;; 参数
;; ----
;; vec : vector?
;; 要计算长度的向量。
;;
;; 返回值
;; ----
;; integer
;; 向量中元素的个数。
;;
;; 注意
;; ----
;; 空向量的长度为0。
;;
;; 示例
;; ----
;; (vector-length #(1 2 3)) => 3
;; (vector-length #()) => 0
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是向量时


(check (vector-length #()) => 0)
(check (vector-length #(42)) => 1)
(check (vector-length #(1 2 3)) => 3)
(check (vector-length #(1 2.5 "hello" (#_quote symbol) #\c #t #f)
       ) ;vector-length
  =>
  7
) ;check
(check-catch 'wrong-type-arg
  (vector-length 'not-a-vector)
) ;check-catch


(check-report)
