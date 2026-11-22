;; cmark-list?
;; 判断对象是否为 list 节点。
;;
;; 语法
;; ----
;; (cmark-list? node)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 list 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-list? (make-cmark-list 'bullet 1 #\-)))
(check-false (cmark-list? (make-cmark-paragraph)))

(check-report)
