;; pi-trunc-result-truncated?
;; 获取文本截断结果中是否发生截断（谓词形式）。
;;
;; 语法
;; ----
;; (pi-trunc-result-truncated? r)
;;
;; 参数
;; ----
;; r : pi-trunc-result?
;; 截断结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若发生截断返回 #t，否则返回 #f。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((tr-yes (pi-trunc-result "text" #t)) (tr-no (pi-trunc-result "text" #f)))
  (check (pi-trunc-result-truncated? tr-yes) => #t)
  (check (pi-trunc-result-truncated? tr-no) => #f)
) ;let

(check-report)
