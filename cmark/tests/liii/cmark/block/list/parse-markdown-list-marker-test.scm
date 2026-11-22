;; parse-markdown-list-marker
;; 将列表项标记行解析为 markdown-list-marker 记录。
;;
;; 语法
;; ----
;; (parse-markdown-list-marker line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; markdown-list-marker 或 #f
;; 成功返回记录，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(let ((m (parse-markdown-list-marker "1. a")))
  (check-true (markdown-list-marker? m))
  (check (markdown-list-marker-type m) => 'ordered)
) ;let

(check-report)
