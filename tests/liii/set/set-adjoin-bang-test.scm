(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-adjoin!
;; 向 set 中添加一个或多个元素（可变操作）。
;;
;; 语法
;; ----
;; (set-adjoin! set element ...)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element ... : any
;; 要添加的元素。
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
;; (set-adjoin! (set) 1) => 修改后的 set，包含 1


(define s-empty (set))


;; Test basic adjoin!
(define s-mut (set-copy s-empty))
(set-adjoin! s-mut 1)
(check (set-size s-mut) => 1)
(check-true (set-contains? s-mut 1))


(set-adjoin! s-mut 2 3)
(check (set-size s-mut) => 3)
(check-true (set-contains? s-mut 1))
(check-true (set-contains? s-mut 2))
(check-true (set-contains? s-mut 3))


;; Test adding existing element
(set-adjoin! s-mut 1)
(check (set-size s-mut) => 3)


;; Test type error
(check-catch 'type-error
  (set-adjoin! "not a set" 1)
) ;check-catch


(check-report)
