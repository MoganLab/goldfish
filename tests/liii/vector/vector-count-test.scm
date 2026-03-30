(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-count
;; 统计向量中满足谓词的元素数量。
;;
;; 语法
;; ----
;; (vector-count pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于判断元素是否计数的谓词。
;;
;; vec : vector?
;; 要统计的向量。
;;
;; 返回值
;; ----
;; integer
;; 满足谓词的元素个数。
;;
;; 注意
;; ----
;; 空向量的统计结果始终为0。
;;
;; 示例
;; ----
;; (vector-count even? #(1 2 3 4)) => 2
;; (vector-count string? #(1 "a" 2)) => 1
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-count even? #()) => 0)
(check (vector-count even? #(1 3 5 7 9)) => 0)
(check (vector-count even? #(1 3 4 7 8)) => 2)
(check (vector-count (lambda (x) #t) #()) => 0)
(check (vector-count (lambda (x) #f) #(1 2 3)) => 0)
(check (vector-count (lambda (x) #t) #(1 2 3)) => 3)
(check (vector-count string? #(1 "a" 2 "b" 3)) => 2)
(check (vector-count number? #(1 "a" 2 "b" 3)) => 3)
(check (vector-count symbol? #(a b 1 2)) => 2)
(check (vector-count even? #(42)) => 1)
(check (vector-count even? #(43)) => 0)
(check (vector-count (lambda (x) (> x 5)) #(1 6 2 7 3 8)) => 3)
(check (vector-count (lambda (x) (char=? x #\a)) #(#\a #\b #\a #\c)) => 2)

(check-report)
