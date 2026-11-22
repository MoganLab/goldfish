;; reference-map?
;; 判断对象是否为 reference-map 实例。
;;
;; 语法
;; ----
;; (reference-map? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 reference-map 返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check-true (reference-map? (make-reference-map)))
(check-false (reference-map? '()))

(check-report)
