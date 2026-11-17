
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-softbreak
;; 创建软换行节点。
;;
;; 语法
;; ----
;; (make-cmark-softbreak)

(let ((node (make-cmark-softbreak)))
  (check (cmark-node-type node) => 'softbreak)
  (check (cmark-node-data node) => '())
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

(check-report)
