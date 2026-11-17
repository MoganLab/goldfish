;; markdown-list-marker-start
;; 获取有序列表标记的起始编号。
;;
;; 语法
;; ----
;; (markdown-list-marker-start m)
;;
;; 参数
;; ----
;; m : markdown-list-marker
;; 列表标记。
;;
;; 返回值
;; ----
;; integer
;; 返回起始编号。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check (markdown-list-marker-start (make-markdown-list-marker 'ordered 0 3 "." "x" 3))
  =>
  3
) ;check

(check-report)
