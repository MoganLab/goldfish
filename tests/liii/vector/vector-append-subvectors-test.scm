(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-append-subvectors
;; 连接多个向量的子向量并返回新向量。
;;
;; 语法
;; ----
;; (vector-append-subvectors vec1 start1 end1 vec2 start2 end2 ...)
;;
;; 参数
;; ----
;; vec  : vector?
;; start: exact-nonnegative-integer?
;; end  : exact-nonnegative-integer?
;; 一个或多个 (vec start end) 三元组。
;;
;; 返回值
;; ----
;; vector
;; 一个新的向量，包含所有指定子向量的元素。
;;
;; 注意
;; ----
;; 至少需要一个三元组；start 和 end 指定半开区间 [start, end)。


(check (vector-append-subvectors #(a b c d e) 0 2 #(f g h i j) 2 4)
  =>
  #(a b h i)
) ;check

(check (vector-append-subvectors #(1 2 3) 0 3) => #(1 2 3))

(check (vector-append-subvectors #(1 2 3) 1 3 #(4 5 6) 0 2) => #(2 3 4 5))

(check (vector-append-subvectors #() 0 0 #() 0 0) => #())

(check (vector-append-subvectors #(a b c) 0 0 #(d e) 1 2) => #(e))


(let ((v1 #(1 2 3)) (v2 #(4 5 6)))
  (let ((result (vector-append-subvectors v1 0 2 v2 1 3)))
    (check result => #(1 2 5 6))
    (vector-set! result 0 99)
    (check v1 => #(1 2 3))
    (check v2 => #(4 5 6))
  ) ;let
) ;let


(check-report)
