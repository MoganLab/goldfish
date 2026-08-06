(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; coverlet
;; 撤销此前 openlet 对一个 let 的 "open" 标记。返回该 let 自身。
;;
;; 语法
;; ----
;; (coverlet let)
;;
;; 参数
;; ----
;; let : let?
;; 要撤销 open 标记的 let。
;;
;; 返回值
;; ------
;; let?
;; 返回传入的 let（已撤销 open 标记）。
;;
;; 说明
;; ----
;; coverlet 是 openlet 的逆操作：
;; openlet 把 let 标记为 open，使内建函数查询其方法；
;; coverlet 撤销该标记，使内建函数不再查询其方法。


;; coverlet 返回传入的 let
(check (let ((e (inlet 'a 1))) (eq? (coverlet e) e)) => #t)


;; openlet 后再 coverlet，openlet? 变回 #f
(check (let ((e (openlet (inlet 'a 1)))) (coverlet e) (openlet? e)) => #f)


;; 未经 openlet 的 let，coverlet 后 openlet? 仍为 #f
(check (openlet? (coverlet (inlet 'a 1))) => #f)


;; coverlet 后内部绑定仍可访问
(check (let-ref (coverlet (openlet (inlet 'a 42))) 'a) => 42)


(check-report)
