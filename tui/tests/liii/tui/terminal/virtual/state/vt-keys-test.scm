;; vt-keys
;; 获取虚拟终端内部记录的按键队列列表。
;;
;; 语法
;; ----
;; (vt-keys st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; list?
;; 按键队列列表。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '(a b))))
  (check (vt-keys st) => '(a b))
) ;let

(check-report)
