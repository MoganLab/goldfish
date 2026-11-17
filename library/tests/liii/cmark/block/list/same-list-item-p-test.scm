;; same-list-item?
;; 判断行与前一列表标记是否属于同类型、同分隔符的列表。
;;
;; 语法
;; ----
;; (same-list-item? line prev-marker)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;; prev-marker : markdown-list-marker
;; 前一列表标记。
;;
;; 返回值
;; ----
;; boolean
;; 同类型列表返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(let ((m (parse-markdown-list-marker "- a")))
  (check-true (same-list-item? "- b" m))
  (check-false (same-list-item? "* b" m))
) ;let

(check-report)
