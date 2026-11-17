;; markdown-list-marker-indent
;; 获取列表标记的缩进字段。
;;
;; 语法
;; ----
;; (markdown-list-marker-indent m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; integer
;; 返回前导空格数。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-indent (make-markdown-list-marker 1 'bullet 1 "-" 3 "x"))
  =>
  1
) ;check

(check-report)
