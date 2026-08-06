(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; varlet
;; 将若干绑定添加到 target-let，并返回 target-let 自身。
;;
;; 语法
;; ----
;; (varlet target-let symbol value)
;; (varlet target-let (symbol . value))
;; (varlet target-let other-let)
;;
;; 参数
;; ----
;; target-let : let?
;; 要添加绑定的目标 let。
;;
;; symbol : symbol?
;; 绑定名。
;;
;; value : any
;; 绑定值。
;;
;; other-let : let?
;; 另一个 let，其所有绑定会被合并到 target-let。
;;
;; 返回值
;; ------
;; let?
;; 返回 target-let（已被就地修改）。
;;
;; 说明
;; ----
;; varlet 就地修改 target-let：把新绑定直接合并进去，
;; 返回的就是 target-let 本身（eq? 为 #t）。
;; 这与 sublet 不同：sublet 创建新 let，不改父。


;; varlet 把绑定添加到 target-let，并返回同一对象
(check (let ((e (inlet 'a 1))) (eq? (varlet e 'b 2) e)) => #t)


;; varlet 添加的绑定可通过 let-ref 访问
(check (let ((e (inlet 'a 1))) (varlet e 'b 99) (let-ref e 'b)) => 99)


;; varlet 保留原有绑定
(check (let ((e (inlet 'a 1))) (varlet e 'b 2) (let-ref e 'a)) => 1)


;; varlet 合并另一个 let 的所有绑定
(check (let ((target (inlet 'a 1)) (source (inlet 'b 2 'c 3)))
         (varlet target source)
         (length target)
       ) ;let
  =>
  3
) ;check


;; varlet 接受点对形式 (symbol . value)
(check (let ((e (inlet))) (varlet e (cons 'x 99)) (let-ref e 'x)) => 99)


(check-report)
