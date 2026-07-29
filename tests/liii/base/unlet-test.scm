(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; unlet
;; 返回一个 let，该 let 反映所有预定义（内建）函数的原始绑定。
;;
;; 语法
;; ----
;; (unlet)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ------
;; let?
;; 包含所有内建函数原始绑定的 let。
;;
;; 说明
;; ----
;; unlet 返回的 let 用于访问被遮蔽的内建函数的原始版本。
;; 典型用法是 (with-let (unlet) ...)：
;; 在该 with-let 内部，即使某个内建函数已被全局 set! 遮蔽，
;; 也能访问到其原始定义。


;; unlet 返回一个 let
(check (let? (unlet)) => #t)


;; unlet 的 let 中包含内建函数 + 的原始绑定
(check (symbol->value '+ (unlet)) => +)


;; 在遮蔽内建函数后，通过 with-let (unlet) 可访问原始版本
(check (let ()
         (set! map (lambda args 'shadowed))
         (with-let (unlet) (map (lambda (x) (* x 10)) (list 1 2 3)))
       ) ;let
  =>
  (list 10 20 30)
) ;check


(check-report)
