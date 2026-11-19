;; vt-cursor-x-set!
;; 设置虚拟终端内部记录的光标列号。
;;
;; 语法
;; ----
;; (vt-cursor-x-set! st x)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; x : integer?
;; 目标列号。
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
  (vt-cursor-x-set! st 7)
  (check (vt-cursor-x st) => 7)
) ;let

(check-report)
