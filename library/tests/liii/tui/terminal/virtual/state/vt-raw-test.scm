;; vt-raw
;; 获取虚拟终端内部记录的原始输出文本片段列表。
;;
;; 语法
;; ----
;; (vt-raw st)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; 返回值
;; ----
;; list?
;; 原始输出文本片段列表。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '("abc") '())))
  (check (vt-raw st) => '("abc"))
) ;let

(check-report)
