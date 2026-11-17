;; append-inline-node
;; 向逆序节点列表中追加行内节点（自动合并相邻 text 节点）。
;;
;; 语法
;; ----
;; (append-inline-node nodes node)
;;
;; 参数
;; ----
;; nodes : list
;; 逆序节点列表。
;; node : cmark-node
;; 待追加节点。
;;
;; 返回值
;; ----
;; list
;; 返回更新后的逆序节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let* ((n1 (make-cmark-text "a"))
       (n2 (make-cmark-text "b"))
       (res (append-inline-node (list n1) n2))
      ) ;
  (check (length res) => 1)
  (check
    (cdr (assoc 'content (cmark-node-data (car res))))
    =>
    "ab"
  ) ;check
) ;let*

(check-report)
