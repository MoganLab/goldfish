;; vt-lines-set!
;; 设置虚拟终端内部记录的屏幕字符行向量。
;;
;; 语法
;; ----
;; (vt-lines-set! st lines)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; lines : vector?
;; 新的屏幕行向量。
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
  (vt-lines-set! st '#(#\a))
  (check (vt-lines st) => '#(#\a))
) ;let

(check-report)
