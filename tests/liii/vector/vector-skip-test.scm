(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-skip
;; 从左到右跳过满足谓词的元素，返回首个不满足谓词的索引。
;;
;; 语法
;; ----
;; (vector-skip pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于判断是否继续跳过当前元素的谓词。
;;
;; vec : vector?
;; 要搜索的向量。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回第一个不满足pred的元素索引；如果所有元素都满足pred，则返回#f。
;;
;; 注意
;; ----
;; 搜索方向为从左到右。
;;
;; 示例
;; ----
;; (vector-skip odd? #(1 3 4 5)) => 2
;; (vector-skip even? #()) => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-skip even? #(1 2 3 4)) => 0)
(check (vector-skip odd? #(1 3 5 7)) => #f)
(check (vector-skip (lambda (x) (< x 5)) #(1 2 3 4 5)) => 4)
(check (vector-skip (lambda (x) (char=? x #\a)) #(#\a #\a #\b #\c)) => 2)
(check (vector-skip even? #()) => #f)
(check (vector-skip even? #(1)) => 0)
(check (vector-skip odd? #(2)) => 0)
(check (vector-skip (lambda (x) (string=? x "a")) #("a" "a" "b" "c")) => 2)
(check (vector-skip (lambda (x) (eq? x #t)) #(#t #t #f #t)) => 2)
(check (vector-skip (lambda (x) (> x 0)) #(1 2 3 4)) => #f)
(check (vector-skip (lambda (x) (char-alphabetic? x)) #(#\a #\b #\c)) => #f)

(check-report)
