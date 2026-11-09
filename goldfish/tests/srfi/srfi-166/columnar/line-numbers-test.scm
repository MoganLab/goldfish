;; line-numbers
;; 行号组合子：每次输出递增一行的序号。
;;
;; 语法
;; ----
;; (line-numbers [start])
;;
;; 参数
;; ----
;; start : exact-integer?
;; 可选，起始行号，默认 1。
;;
;; 返回值
;; ----
;; format?
;; 每次求值输出一个行号、并在下次递增的组合子。
;;
;; 说明
;; ----
;; 计数状态藏在组合子内部，通常作为 columnar 的第一列，
;; 与 from-file 等内容列并排使用。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check (show #f (line-numbers)) => "1")
(check (show #f (line-numbers 5)) => "5")
(check (show #f (columnar 3 'right (line-numbers 5) " " (each "a\nb\n")))
  =>
  "  5 a\n    b\n"
) ;check

(check-report)
