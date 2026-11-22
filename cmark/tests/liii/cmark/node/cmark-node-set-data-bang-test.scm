;; cmark-node-set-data!
;; 修改 cmark-node 的附加数据 alist。
;;
;; 语法
;; ----
;; (cmark-node-set-data! node data)
;;
;; 参数
;; ----
;; node : cmark-node
;; AST 节点。
;; data : alist
;; 新的附加数据 alist。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新节点附加数据。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((n (make-cmark-paragraph)))
  (cmark-node-set-data! n '((content . "hi")))
  (check (cmark-node-data n) => '((content . "hi")))
) ;let

(check-report)
