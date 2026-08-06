(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; let-ref
;; 返回 let 中指定符号的绑定值。
;;
;; 语法
;; ----
;; (let-ref let sym)
;;
;; 参数
;; ----
;; let : let?
;; 要查询的 let。
;;
;; sym : symbol?
;; 要查询的符号。
;;
;; 返回值
;; ------
;; any
;; 该符号在 let 中的绑定值。
;;
;; 说明
;; ----
;; let-ref 从 let 中查找指定符号的值。
;; 若 let 中没有该符号的绑定，返回 #<undefined>。
;; let-ref 只查找 let 的直接绑定，不递归到 outlet。
;; rootlet-ref 是 let-ref 的快速路径变体（针对 rootlet）。


;; 从 inlet 中获取绑定
(check (let-ref (inlet 'a 42) 'a) => 42)


;; 获取字符串绑定
(check (let-ref (inlet 'name "hello") 'name) => "hello")


;; 多绑定的 let 中获取指定符号
(check (let-ref (inlet 'a 1 'b 2 'c 3) 'b) => 2)


;; 未绑定的符号返回 #<undefined>
(check (undefined? (let-ref (inlet 'a 1) 'z)) => #t)


;; let-ref 只查直接绑定，不递归 outlet
(check (let ((parent (inlet 'a 1))) (let-ref (sublet parent 'b 2) 'a)) => 1)


;; 从 rootlet 中获取内建函数 +
(check (let-ref (rootlet) '+) => +)


(check-report)
