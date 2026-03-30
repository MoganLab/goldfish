(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-filter
;; 根据谓词筛选向量元素。
;;
;; 语法
;; ----
;; (vector-filter pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于筛选元素的谓词。
;;
;; vec : vector?
;; 要筛选的向量。
;;
;; 返回值
;; ----
;; vector
;; 一个只包含满足pred元素的新向量。
;;
;; 注意
;; ----
;; 结果向量中的元素顺序与原向量保持一致。
;;
;; 示例
;; ----
;; (vector-filter even? #(1 2 3 4 5 6)) => #(2 4 6)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-filter even? #(1 2 3 4 5 6)) => #(2 4 6))
(check (vector-filter (lambda (x) (> x 3)) #(1 2 3 4 5 6)) => #(4 5 6))
(check (vector-filter (lambda (x) (string? x)) #(1 "a" 2 "b" 3)) => #("a" "b"))
(check (vector-filter (lambda (x) #t) #()) => #())
(check (vector-filter (lambda (x) #f) #(1 2 3)) => #())

(check-report)
