;; trim-trailing-spaces-from-last-text-node
;; 去除逆序节点列表首元素（最后一个文本节点）末尾的空格。
;;
;; 语法
;; ----
;; (trim-trailing-spaces-from-last-text-node nodes)
;;
;; 参数
;; ----
;; nodes : list
;; 逆序节点列表。
;;
;; 返回值
;; ----
;; list
;; 返回更新后的节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark inline util))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((res (trim-trailing-spaces-from-last-text-node (list (make-cmark-text "hi  "))))
     ) ;
  (check
    (cdr (assoc 'content (cmark-node-data (car res))))
    =>
    "hi"
  ) ;check
) ;let

(check-report)
