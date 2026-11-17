;; cmark-heading?
;; 判断对象是否为 heading 节点。
;;
;; 语法
;; ----
;; (cmark-heading? node)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 heading 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-heading? (make-cmark-heading 1)))
(check-false (cmark-heading? (make-cmark-paragraph)))

(check-report)
