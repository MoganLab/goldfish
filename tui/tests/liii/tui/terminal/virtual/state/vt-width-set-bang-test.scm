;; vt-width-set!
;; 设置虚拟终端内部记录的宽度。
;;
;; 语法
;; ----
;; (vt-width-set! st w)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; w : integer?
;; 宽度。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (vt-width-set! st 100)
  (check (vt-width st) => 100)
) ;let

(check-report)
