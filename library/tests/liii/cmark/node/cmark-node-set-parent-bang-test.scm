;; cmark-node-set-parent!
;; 设置 cmark-node 的父节点。
;;
;; 语法
;; ----
;; (cmark-node-set-parent! node parent)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;; parent : cmark-node 或 #f
;; 父节点引用。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新父节点引用。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((p (make-cmark-paragraph)) (t (make-cmark-text "a")))
  (cmark-node-set-parent! t p)
  (check (cmark-node-parent t) => p)
) ;let

(check-report)
