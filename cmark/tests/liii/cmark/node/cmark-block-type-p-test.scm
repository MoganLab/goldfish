;; cmark-block-type?
;; 判断类型符号是否为块级节点类型。
;;
;; 语法
;; ----
;; (cmark-block-type? type)
;;
;; 参数
;; ----
;; type : symbol
;; 节点类型符号。
;;
;; 返回值
;; ----
;; boolean
;; 属于块级节点类型返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-block-type? 'document))
(check-true (cmark-block-type? 'paragraph))
(check-false (cmark-block-type? 'text))

(check-report)
