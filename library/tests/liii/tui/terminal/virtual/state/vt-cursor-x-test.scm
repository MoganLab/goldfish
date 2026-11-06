;; vt-cursor-x
;; 获取虚拟终端内部记录的光标列号。
;;
;; 语法
;; ----
;; (vt-cursor-x st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; integer?
;; 光标列号。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 5 10 #t '#() '() '())))
  (check (vt-cursor-x st) => 5)
) ;let

(check-report)
