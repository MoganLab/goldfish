;; vt-visible?
;; 获取虚拟终端内部记录的光标可见性。
;;
;; 语法
;; ----
;; (vt-visible? st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; boolean?
;; 光标是否可见。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check-true (vt-visible? st))
) ;let

(check-report)
