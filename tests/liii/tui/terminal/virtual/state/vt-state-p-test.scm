;; vt-state?
;; 判断给定对象是否为虚拟终端状态记录。
;;
;; 语法
;; ----
;; (vt-state? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为虚拟终端状态记录返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check-true (vt-state? st))
) ;let

(check-false (vt-state? "not-state"))
(check-false (vt-state? 123))

(check-report)
