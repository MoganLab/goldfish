(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-adjoin
;; 返回一个新的 set，包含原 set 的所有元素以及新增的元素。
;;
;; 语法
;; ----
;; (set-adjoin set element ...)
;;
;; 参数
;; ----
;; set : set
;; 初始 set。
;;
;; element ... : any
;; 要添加的元素。
;;
;; 返回值
;; ------
;; set
;; 返回一个新的 set。
;;
;; 注意
;; ----
;; 此函数不修改原 set。
;;
;; 示例
;; ----
;; (set-adjoin (set) 1) => 包含 1 的 set
;; (set-adjoin (set 1) 2 3) => 包含 1, 2, 3 的 set

(define s-empty (set))

;; Test basic adjoin
(define s-adjoin-1 (set-adjoin s-empty 1))
(check (set-size s-adjoin-1) => 1)
(check-true (set-contains? s-adjoin-1 1))
(check-true (set-empty? s-empty)) ; Original set unchanged

(define s-adjoin-2 (set-adjoin (set 1) 2 3))
(check (set-size s-adjoin-2) => 3)
(check-true (set-contains? s-adjoin-2 1))
(check-true (set-contains? s-adjoin-2 2))
(check-true (set-contains? s-adjoin-2 3))

;; Test adding existing element
(define s-adjoin-3 (set-adjoin (set 1) 1))
(check (set-size s-adjoin-3) => 1)
(check-true (set-contains? s-adjoin-3 1))

;; Test type error
(check-catch 'type-error (set-adjoin "not a set" 1))

(check-report)
