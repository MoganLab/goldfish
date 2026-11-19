;; vt-cursor-y-set!
;; 设置虚拟终端内部记录的光标行号。
;;
;; 语法
;; ----
;; (vt-cursor-y-set! st y)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; y : integer?
;; 目标行号。
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
  (vt-cursor-y-set! st 8)
  (check (vt-cursor-y st) => 8)
) ;let

(check-report)
