;; reader-newlines
;; 将换行数量转换为换行符串。
;;
;; 语法
;; ----
;; (reader-newlines count)
;;
;; 参数
;; ----
;; count : integer?
;; 换行数量。
;;
;; 返回值
;; ----
;; string?
;; 由 count 个换行符组成的字符串。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-newlines 0) => "")
(check (reader-newlines 2) => "\n\n")

(check-report)
