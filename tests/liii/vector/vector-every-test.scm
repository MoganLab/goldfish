(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-every
;; 判断向量中是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (vector-every pred vec)
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
;; 当所有元素都满足谓词时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 空向量按约定返回#t。
;;
;; 示例
;; ----
;; (vector-every odd? #(1 3 5)) => #t
;; (vector-every odd? #(1 2 3)) => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时

(check (vector-every odd? #()) => #t)
(check (vector-every odd? #(1 3 5 7 9)) => #t)
(check (vector-every odd? #(1 3 4 7 8)) => #f)

(check-report)
