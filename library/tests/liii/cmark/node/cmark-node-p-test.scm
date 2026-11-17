;; cmark-node?
;; 判断对象是否为 cmark-node 记录实例。
;;
;; 语法
;; ----
;; (cmark-node? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 cmark-node 返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-node? (make-cmark-paragraph)))
(check-false (cmark-node? "not-a-node"))
(check-false (cmark-node? 42))

(check-report)
