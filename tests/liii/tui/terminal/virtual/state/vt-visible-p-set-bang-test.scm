;; vt-visible?-set!
;; 设置虚拟终端内部记录的光标可见性。
;;
;; 语法
;; ----
;; (vt-visible?-set! st visible?)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; visible? : boolean?
;; 光标是否可见。
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
  (vt-visible?-set! st #f)
  (check-false (vt-visible? st))
) ;let

(check-report)
