;; cmark-paragraph?
;; 判断对象是否为 paragraph 节点。
;;
;; 语法
;; ----
;; (cmark-paragraph? node)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 paragraph 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-paragraph? (make-cmark-paragraph)))
(check-false (cmark-paragraph? (make-cmark-document)))

(check-report)
