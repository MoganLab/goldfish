;; table-start?
;; 判断行列表是否以合法的 GFM 表格表头与对齐行起始。
;;
;; 语法
;; ----
;; (table-start? lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; boolean
;; 是表格起始返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block table))

(check-set-mode! 'report-failed)

(check-true (table-start? '("| a | b |" "| --- | --- |")))
(check-false (table-start? '("| a | b |" "plain")))

(check-report)
