;; vt-width
;; 获取虚拟终端内部记录的宽度。
;;
;; 语法
;; ----
;; (vt-width st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; integer?
;; 宽度。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check (vt-width st) => 80)
) ;let

(check-report)
