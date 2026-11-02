;; pi-trunc-result-first-line-truncated
;; 获取截断结果的首行是否已超限截断。
;;
;; 语法
;; ----
;; (pi-trunc-result-first-line-truncated r)
;;
;; 参数
;; ----
;; r : pi-trunc-result?
;; 截断结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; 首行是否超限截断。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((tr (pi-trunc-result "text" #t 'bytes 1 100 1 50 #t #f)))
  (check (pi-trunc-result-first-line-truncated tr) => #t)
) ;let

(check-report)
