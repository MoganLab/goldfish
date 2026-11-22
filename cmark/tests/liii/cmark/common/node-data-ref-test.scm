;; node-data-ref
;; 从 cmark-node 的 data 中按键取值，缺省返回 default。
;;
;; 语法
;; ----
;; (node-data-ref node key default)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;; key : symbol
;; 键名。
;; default : any
;; 默认值。
;;
;; 返回值
;; ----
;; any
;; 返回字段值或默认值。

(import (scheme base))
(import (liii check))
(import (liii cmark common))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (node-data-ref (make-cmark-heading 2) 'level 1) => 2)
(check (node-data-ref (make-cmark-paragraph) 'level 1) => 1)

(check-report)
