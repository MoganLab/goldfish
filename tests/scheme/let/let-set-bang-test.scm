(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; let-set!
;; 设置 let 中指定符号的绑定值（就地修改）。
;;
;; 语法
;; ----
;; (let-set! let sym val)
;;
;; 参数
;; ----
;; let : let?
;; 要修改的 let。
;;
;; sym : symbol?
;; 要设置的符号。
;;
;; val : any
;; 新的绑定值。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未指定（副作用操作）。
;;
;; 说明
;; ----
;; let-set! 就地修改 let 中指定符号的绑定值。
;; 若 let 中已有该符号，则更新其值；否则行为依实现而定。
;; 修改后通过 let-ref 可读到新值。


;; let-set! 更新已有绑定
(check (let ((e (inlet 'a 1))) (let-set! e 'a 99) (let-ref e 'a)) => 99)


;; let-set! 设置新值后，原值被覆盖
(check (let ((e (inlet 'a 1 'b 2))) (let-set! e 'a 100) (let-ref e 'b)) => 2)


;; let-set! 可设置字符串值
(check (let ((e (inlet 'name "old")))
         (let-set! e 'name "new")
         (let-ref e 'name)
       ) ;let
  =>
  "new"
) ;check


;; let-set! 可设置列表值
(check (let ((e (inlet 'items (list 1 2))))
         (let-set! e 'items (list 1 2 3))
         (let-ref e 'items)
       ) ;let
  =>
  (list 1 2 3)
) ;check


;; let-set! 可设置 #f / #t
(check (let ((e (inlet 'flag #t)))
         (let-set! e 'flag #f)
         (let-ref e 'flag)
       ) ;let
  =>
  #f
) ;check


(check-report)
