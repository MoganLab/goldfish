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

(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)

(check
  (let ((e (inlet 'a 1 'b 2 'c 3)))
    (cutlet e 'b)
    (length e)
  ) ;let
  =>
  2
) ;check


;; cutlet 返回的就是传入的 let（eq? 为 #t）
(check
  (let ((e (inlet 'a 1)))
    (eq? (cutlet e 'a) e)
  ) ;let
  =>
  #t
) ;check


;; cutlet 一次移除多个符号
(check
  (let ((e (inlet 'a 1 'b 2 'c 3)))
    (cutlet e 'a 'c)
    (length e)
  ) ;let
  =>
  1
) ;check


;; cutlet 后剩余的绑定仍可访问
(check
  (let ((e (inlet 'a 1 'b 2 'c 3)))
    (cutlet e 'b)
    (let-ref e 'a)
  ) ;let
  =>
  1
) ;check


(check-report)
