;; vt-keys-set!
;; 设置虚拟终端内部记录的按键队列列表。
;;
;; 语法
;; ----
;; (vt-keys-set! st keys)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; keys : list?
;; 新的按键列表。
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
  (vt-keys-set! st '(x y))
  (check (vt-keys st) => '(x y))
) ;let

(check-report)
