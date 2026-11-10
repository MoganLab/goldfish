;; pi-trunc-result-last-line-truncated
;; 获取截断结果的末行是否被部分截断。
;;
;; 语法
;; ----
;; (pi-trunc-result-last-line-truncated r)
;;
;; 参数
;; ----
;; r : pi-trunc-result?
;; 截断结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; 末行是否被部分截断。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((tr (pi-trunc-result "text" #t 'bytes 2 200 2 150 #f #t)))
  (check (pi-trunc-result-last-line-truncated tr) => #t)
) ;let

(check-report)
