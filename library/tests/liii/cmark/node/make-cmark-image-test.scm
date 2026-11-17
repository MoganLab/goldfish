(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-image
;; 创建图片节点。
;;
;; 语法
;; ----
;; (make-cmark-image url title)

(let ((node (make-cmark-image "img.png" "Alt")))
  (check (cmark-node-type node) => 'image)
  (check (cdr (assoc 'url (cmark-node-data node))) => "img.png")
  (check (cdr (assoc 'title (cmark-node-data node))) => "Alt")
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空 URL、空 title 与空 alt
(let ((node (make-cmark-image "" "")))
  (check (cdr (assoc 'url (cmark-node-data node))) => "")
  (check (cdr (assoc 'title (cmark-node-data node))) => "")
) ;let

;; 测试带子节点的图片（alt 文本通过子节点表达）
(let ((node (make-cmark-image "logo.png" "Logo Title")))
  (let ((alt1 (make-cmark-text "Alt ")))
    (cmark-node-append-child! node alt1)
    (let ((alt2 (make-cmark-text "Text")))
      (cmark-node-append-child! node alt2)
      (check (length (cmark-node-children node)) => 2)
      (check (cmark-node-parent alt1) => node)
      (check (cmark-node-parent alt2) => node)
      (check (cmark-node-type (cadr (cmark-node-children node))) => 'text)
    ) ;let
  ) ;let
) ;let

;; 测试复杂图片 URL
(let ((node (make-cmark-image "https://cdn.example.com/img/v2/logo.png?size=large" "")
      ) ;node
     ) ;
  (check (cdr (assoc 'url (cmark-node-data node)))
    =>
    "https://cdn.example.com/img/v2/logo.png?size=large"
  ) ;check
) ;let

(check-report)
