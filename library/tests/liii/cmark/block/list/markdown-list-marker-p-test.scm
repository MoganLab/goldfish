;; markdown-list-marker?
;; 判断对象是否为 markdown-list-marker 实例。
;;
;; 语法
;; ----
;; (markdown-list-marker? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是实例返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(check-true (markdown-list-marker? (make-markdown-list-marker 'bullet 0 1 "-" "x" 2))
) ;check-true
(check-false (markdown-list-marker? '()))

(check-report)
