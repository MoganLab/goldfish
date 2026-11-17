;; markdown-list-marker-body
;; 获取列表标记后的首行内容。
;;
;; 语法
;; ----
;; (markdown-list-marker-body m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; string
;; 返回内容文本。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-body (make-markdown-list-marker 0 'bullet 1 "-" 2 "item"))
  =>
  "item"
) ;check

(check-report)
