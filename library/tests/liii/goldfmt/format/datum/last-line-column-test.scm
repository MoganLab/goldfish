;; last-line-column
;; 计算文本最后一行的列数。
;;
;; 语法
;; ----
;; (last-line-column text)
;;
;; 参数
;; ----
;; text : string?
;; 待计算的文本。
;;
;; 返回值
;; ----
;; integer?
;; 最后一行的列数。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (last-line-column "abc") => 3)
(check (last-line-column "ab\ncd") => 2)

(check-report)
