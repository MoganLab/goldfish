(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; openlet
;; 标记一个 let 为 "open"，使内建函数会查询该 let 是否覆盖了某个方法。
;; 返回该 let 自身。
;;
;; 语法
;; ----
;; (openlet let)
;;
;; 参数
;; ----
;; let : let?
;; 要标记为 open 的 let。
;;
;; 返回值
;; ------
;; let?
;; 返回传入的 let（已被标记为 open）。
;;
;; 说明
;; ----
;; openlet 用于对象系统：当一个 let 被 open 后，
;; 内建函数（如 +、display 等）在作用于该 let 的内容时，
;; 会先查询该 let 是否定义了同名的方法，若有则调用之。
;; coverlet 可撤销 openlet 的效果。
;; openlet 就地修改并返回原 let。


;; openlet 返回传入的 let（eq? 为 #t）
(check (let ((e (inlet 'a 1))) (eq? (openlet e) e)) => #t)


;; openlet 标记后，openlet? 返回 #t
(check (let ((e (inlet 'a 1))) (openlet e) (openlet? e)) => #t)


;; 未经 openlet 的 inlet，openlet? 为 #f
(check (openlet? (inlet 'a 1)) => #f)


;; openlet 后内部绑定仍可访问
(check (let-ref (openlet (inlet 'a 42)) 'a) => 42)


(check-report)
