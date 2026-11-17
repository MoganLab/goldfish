;; cmark-text?
;; 判断对象是否为 text 节点。
;;
;; 语法
;; ----
;; (cmark-text? node)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 text 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-text? (make-cmark-text "hi")))
(check-false (cmark-text? (make-cmark-paragraph)))

(check-report)
