
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-alert
;; 创建告警块节点。
;;
;; 语法
;; ----
;; (make-cmark-alert kind)

(let ((node (make-cmark-alert 'note)))
  (check (cmark-node-type node) => 'alert)
  (check (assoc 'kind (cmark-node-data node)) => '(kind . note))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试全部五种告警类型
(let ((tip (make-cmark-alert 'tip)))
  (check (cmark-node-type tip) => 'alert)
  (check (assoc 'kind (cmark-node-data tip)) => '(kind . tip))
) ;let

(let ((important (make-cmark-alert 'important)))
  (check (assoc 'kind (cmark-node-data important)) => '(kind . important))
) ;let

(let ((warning (make-cmark-alert 'warning)))
  (check (assoc 'kind (cmark-node-data warning)) => '(kind . warning))
) ;let

(let ((caution (make-cmark-alert 'caution)))
  (check (assoc 'kind (cmark-node-data caution)) => '(kind . caution))
) ;let

(check-report)
