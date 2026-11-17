;; node->stem-list
;; 将单个 cmark 行内节点转换为 md-stem 元素列表。
;;
;; 语法
;; ----
;; (node->stem-list node)
;;
;; 参数
;; ----
;; node : cmark-node
;; 行内 AST 节点。
;;
;; 返回值
;; ----
;; list
;; 返回 md-stem 元素列表。

(import (scheme base))
(import (liii check))
(import (liii cmark stem inline))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (node->stem-list (make-cmark-text "hi")) => '("hi"))
(check (node->stem-list (make-cmark-code "x")) => '((code "x")))

(check-report)
