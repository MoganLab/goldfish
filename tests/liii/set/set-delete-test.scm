(import (liii check)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-delete
;; 返回一个新的 set，其中指定的元素被移除。
;;
;; 语法
;; ----
;; (set-delete set element ...)
;;
;; 参数
;; ----
;; set : set
;; 初始 set。
;;
;; element ... : any
;; 要移除的元素。
;;
;; 返回值
;; ------
;; set
;; 返回一个新的 set。
;; 如果元素不存在，则忽略。
;;
;; 注意
;; ----
;; 此函数不修改原 set。
;;
;; 示例
;; ----
;; (set-delete (set 1 2 3) 1) => 包含 2, 3 的 set
;; (set-delete (set 1 2 3) 1 2) => 包含 3 的 set

(define s-1-2-3 (set 1 2 3))

;; Test basic delete
(define s-del-1 (set-delete s-1-2-3 1))
(check (set-size s-del-1) => 2)
(check-false (set-contains? s-del-1 1))
(check-true (set-contains? s-del-1 2))
(check-true (set-contains? s-del-1 3))

;; Test deleting non-existing element
(define s-del-2 (set-delete s-1-2-3 4))
(check (set-size s-del-2) => 3)
(check-true (set=? s-del-2 s-1-2-3))

;; Test deleting multiple elements
(define s-del-3 (set-delete s-1-2-3 1 2))
(check (set-size s-del-3) => 1)
(check-false (set-contains? s-del-3 1))
(check-false (set-contains? s-del-3 2))
(check-true (set-contains? s-del-3 3))

(check-report)
