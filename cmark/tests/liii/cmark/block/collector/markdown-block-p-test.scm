;; markdown-block?
;; 判断对象是否为 markdown-block 记录。
;;
;; 语法
;; ----
;; (markdown-block? obj)
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
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(check-true (markdown-block? (make-markdown-block 'paragraph '() '())))
(check-false (markdown-block? '()))

(check-report)
