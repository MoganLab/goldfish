;; parse-text
;; 把普通文本包装成 text 节点。
;;
;; 语法
;; ----
;; (parse-text str)
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((text-node (parse-text "Hello world")))
  (check (cmark-node-type text-node) => 'text)
  (check (cdr (assoc 'content (cmark-node-data text-node))) => "Hello world")
) ;let

(let ((text-node (parse-text "")))
  (check (cmark-node-type text-node) => 'text)
  (check (cdr (assoc 'content (cmark-node-data text-node))) => "")
) ;let

(check-report)
