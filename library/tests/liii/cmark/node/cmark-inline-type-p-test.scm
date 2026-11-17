;; cmark-inline-type?
;; 判断类型符号是否为行内节点类型。
;;
;; 语法
;; ----
;; (cmark-inline-type? type)
;;
;; 参数
;; ----
;; type : symbol
;; 节点类型符号。
;;
;; 返回值
;; ----
;; boolean
;; 属于行内节点类型返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-inline-type? 'text))
(check-true (cmark-inline-type? 'link))
(check-false (cmark-inline-type? 'paragraph))

(check-report)
