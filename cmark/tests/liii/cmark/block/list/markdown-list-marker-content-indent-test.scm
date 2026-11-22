;; markdown-list-marker-content-indent
;; 获取列表项内容的对齐缩进列数。
;;
;; 语法
;; ----
;; (markdown-list-marker-content-indent m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; integer
;; 返回内容缩进。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-content-indent (make-markdown-list-marker 0 'bullet 1 "-" 2 "x")
       ) ;markdown-list-marker-content-indent
  =>
  2
) ;check

(check-report)
