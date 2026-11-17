;; collect-table-block
;; 收集完整的 GFM 表格块结构及剩余行。
;;
;; 语法
;; ----
;; (collect-table-block lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回 (table-block-spec . remaining-lines)。

(import (scheme base))
(import (liii check))
(import (liii cmark block table))

(check-set-mode! 'report-failed)

(let ((res (collect-table-block '("| a | b |" "| --- | --- |" "| 1 | 2 |"
                                  "after"))))
  (check (cdr res) => '("after"))
) ;let

(check-report)
