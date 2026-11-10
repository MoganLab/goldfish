;; vt-lines
;; 获取虚拟终端内部记录的屏幕字符行向量。
;;
;; 语法
;; ----
;; (vt-lines st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; vector?
;; 屏幕字符行向量。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check (vector? (vt-lines st)) => #t)
) ;let

(check-report)
