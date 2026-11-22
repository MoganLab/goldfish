;; make-markdown-list-marker
;; 构造 markdown-list-marker 记录实例。
;;
;; 语法
;; ----
;; (make-markdown-list-marker type indent start delimiter body content-indent)
;;
;; 参数
;; ----
;; type : symbol
;; 列表类型。
;; indent : integer
;; 标记缩进。
;; start : integer
;; 起始编号。
;; delimiter : string
;; 分隔符。
;; body : string
;; 内容文本。
;; content-indent : integer
;; 内容缩进。
;;
;; 返回值
;; ----
;; markdown-list-marker
;; 返回标记记录。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(let ((m (make-markdown-list-marker 'bullet 0 1 "-" "item" 2)))
  (check-true (markdown-list-marker? m))
) ;let

(check-report)
