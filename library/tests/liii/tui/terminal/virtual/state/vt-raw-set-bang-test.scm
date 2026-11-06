;; vt-raw-set!
;; 设置虚拟终端内部记录的原始输出文本片段列表。
;;
;; 语法
;; ----
;; (vt-raw-set! st raw)
;;
;; 参数
;; ----
;; st : vt-state?
;; 虚拟终端状态记录。
;;
;; raw : list?
;; 新的原始输出片段列表。
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
  (vt-raw-set! st '("x" "y"))
  (check (vt-raw st) => '("x" "y"))
) ;let

(check-report)
