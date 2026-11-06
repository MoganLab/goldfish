;; reader-append-selected
;; 将首行选中的表单追加到结果文本中。
;;
;; 语法
;; ----
;; (reader-append-selected result selected)
;;
;; 参数
;; ----
;; result : string?
;; 已生成的结果文本。
;;
;; selected : list?
;; 选中项列表。
;;
;; 返回值
;; ----
;; string?
;; 追加选中项后的结果文本。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-append-selected "(" (list (list 'a 1 "a") (list 'b 2 "b")))
  =>
  "( a b"
) ;check
(check (reader-append-selected "(" '()) => "(")

(check-report)
