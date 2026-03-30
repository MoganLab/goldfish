(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-any
;; 判断向量中是否存在满足谓词的元素。
;;
;; 语法
;; ----
;; (vector-any pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于判断元素的谓词。
;;
;; vec : vector?
;; 要检查的向量。
;;
;; 返回值
;; ----
;; boolean
;; 只要存在一个元素满足谓词就返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 空向量的结果为#f。
;;
;; 示例
;; ----
;; (vector-any even? #(1 3 4)) => #t
;; (vector-any even? #()) => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-any even? #()) => #f)
(check (vector-any even? #(1 3 5 7 9)) => #f)
(check (vector-any even? #(1 3 4 7 8)) => #t)

(check-report)
