(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; cutlet
;; 从 let 中移除指定的符号绑定（就地修改），返回该 let。
;;
;; 语法
;; ----
;; (cutlet let symbol ...)
;;
;; 参数
;; ----
;; let : let?
;; 要从中移除绑定的 let。
;;
;; symbol : symbol?
;; 要移除的绑定名（可多个）。
;;
;; 返回值
;; ------
;; let?
;; 返回传入的 let（已被就地修改）。
;;
;; 说明
;; ----
;; cutlet 就地从 let 中移除指定的符号绑定，返回该 let 本身。
;; 可一次移除多个符号。


;; cutlet 移除指定符号
(check (let ((e (inlet 'a 1 'b 2 'c 3))) (cutlet e 'b) (length e)) => 2)


;; cutlet 返回的就是传入的 let（eq? 为 #t）
(check (let ((e (inlet 'a 1))) (eq? (cutlet e 'a) e)) => #t)


;; cutlet 一次移除多个符号
(check (let ((e (inlet 'a 1 'b 2 'c 3))) (cutlet e 'a 'c) (length e)) => 1)


;; cutlet 后剩余的绑定仍可访问
(check (let ((e (inlet 'a 1 'b 2 'c 3))) (cutlet e 'b) (let-ref e 'a)) => 1)


(check-report)
