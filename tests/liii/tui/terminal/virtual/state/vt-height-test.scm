;; vt-height
;; 获取虚拟终端内部记录的高度。
;;
;; 语法
;; ----
;; (vt-height st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; integer?
;; 高度。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check (vt-height st) => 24)
) ;let

(check-report)
