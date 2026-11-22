;; nodes->stem-list
;; 将 cmark 行内 AST 节点列表转换为 md-stem 列表。
;;
;; 语法
;; ----
;; (nodes->stem-list nodes)
;;
;; 参数
;; ----
;; nodes : list
;; cmark-node 列表。
;;
;; 返回值
;; ----
;; list
;; 返回 md-stem 行内元素列表。

(import (scheme base))
(import (liii check))
(import (liii cmark stem inline))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (nodes->stem-list (list (make-cmark-text "hi") (make-cmark-code "x")))
  =>
  '("hi" (code "x"))
) ;check

(check-report)
