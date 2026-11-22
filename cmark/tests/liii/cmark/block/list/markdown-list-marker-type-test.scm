;; markdown-list-marker-type
;; 获取列表标记的类型字段。
;;
;; 语法
;; ----
;; (markdown-list-marker-type m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; symbol
;; 返回 bullet 或 ordered。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-type (make-markdown-list-marker 0 'bullet 1 "-" 2 "x"))
  =>
  'bullet
) ;check

(check-report)
