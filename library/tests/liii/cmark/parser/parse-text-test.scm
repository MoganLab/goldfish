
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline-parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-text
;; 把普通文本包装成 text 节点。
;;
;; 语法
;; ----
;; (parse-text str)

(let ((text-node (parse-text "Hello world")))
  (check (cmark-node-type text-node) => 'text)
  (check (cdr (assoc 'content (cmark-node-data text-node))) => "Hello world")
) ;let

(let ((text-node (parse-text "")))
  (check (cmark-node-type text-node) => 'text)
  (check (cdr (assoc 'content (cmark-node-data text-node))) => "")
) ;let

(check-report)
