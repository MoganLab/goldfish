;; make-cmark-link
;; 创建链接节点。
;;
;; 语法
;; ----
;; (make-cmark-link url title)
;;
;; 参数
;; ----
;; url : any
;; 输入参数。
;; title : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回新创建的 cmark AST 节点。

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((node (make-cmark-link "http://example.com" "Example")))
  (check (cmark-node-type node) => 'link)
  (check (cdr (assoc 'url (cmark-node-data node))) => "http://example.com")
  (check (cdr (assoc 'title (cmark-node-data node))) => "Example")
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空 URL 与空 title
(let ((node (make-cmark-link "" "")))
  (check (cdr (assoc 'url (cmark-node-data node))) => "")
  (check (cdr (assoc 'title (cmark-node-data node))) => "")
) ;let

;; 测试带子节点的链接
(let ((node (make-cmark-link "https://example.com" "Title")))
  (let ((child (make-cmark-text "link text")))
    (cmark-node-append-child! node child)
    (check (length (cmark-node-children node)) => 1)
    (check (cmark-node-parent child) => node)
    (check (cmark-node-type (car (cmark-node-children node))) => 'text)
  ) ;let
) ;let

;; 测试复杂 URL（含查询参数与实体）
(let ((node (make-cmark-link "https://example.com?a=1&b=2" "T & T")))
  (check (cdr (assoc 'url (cmark-node-data node)))
    =>
    "https://example.com?a=1&b=2"
  ) ;check
  (check (cdr (assoc 'title (cmark-node-data node))) => "T & T")
) ;let

(check-report)
