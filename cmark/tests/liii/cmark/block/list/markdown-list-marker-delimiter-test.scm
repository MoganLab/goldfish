;; markdown-list-marker-delimiter
;; 获取列表标记的分隔符。
;;
;; 语法
;; ----
;; (markdown-list-marker-delimiter m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; string
;; 返回分隔符字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-delimiter (make-markdown-list-marker 'ordered 0 1 "." "x" 3)
       ) ;markdown-list-marker-delimiter
  =>
  "."
) ;check

(check-report)
