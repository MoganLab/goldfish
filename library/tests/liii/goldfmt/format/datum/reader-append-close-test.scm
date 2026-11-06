;; reader-append-close
;; 为结果文本追加闭合括号。
;;
;; 语法
;; ----
;; (reader-append-close result close-indent)
;;
;; 参数
;; ----
;; result : string?
;; 已生成的结果文本。
;;
;; close-indent : integer?
;; 闭合括号的缩进列。
;;
;; 返回值
;; ----
;; string?
;; 追加闭合括号后的结果文本。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-append-close "(a" 0) => "(a)")
(check (reader-append-close "(a ;#" 2) => "(a ;#\n  )")

(check-report)
