;; cmark-document?
;; 判断对象是否为 document 节点。
;;
;; 语法
;; ----
;; (cmark-document? node)
;;
;; 参数
;; ----
;; node : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 是 document 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-document? (make-cmark-document)))
(check-false (cmark-document? (make-cmark-paragraph)))

(check-report)
