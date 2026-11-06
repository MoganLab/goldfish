;; vt-height-set!
;; 设置虚拟终端内部记录的高度。
;;
;; 语法
;; ----
;; (vt-height-set! st h)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; h : integer?
;; 高度。
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
  (vt-height-set! st 30)
  (check (vt-height st) => 30)
) ;let

(check-report)
