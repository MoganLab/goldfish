(import (liii check) (liii set))


(check-set-mode! 'report-failed)


;; set-delete!
;; 从 set 中移除指定的元素（可变操作）。
;;
;; 语法
;; ----
;; (set-delete! set element ...)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element ... : any
;; 要移除的元素。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set（与传入的 set 是同一个对象）。
;;
;; 注意
;; ----
;; 此函数会修改原 set。
;;
;; 示例
;; ----
;; (set-delete! (set 1 2 3) 1) => 修改后的 set，包含 2, 3


(define s-1-2-3 (set 1 2 3))


;; Test basic delete!
(define s-mut-del (set-copy s-1-2-3))
(set-delete! s-mut-del 1)
(check (set-size s-mut-del) => 2)
(check-false (set-contains? s-mut-del 1)
) ;check-false


(set-delete! s-mut-del 2 3)
(check (set-size s-mut-del) => 0)
(check-true (set-empty? s-mut-del))


(check-report)
