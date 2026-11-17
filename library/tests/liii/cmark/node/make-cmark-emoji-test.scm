
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-emoji
;; 创建 emoji 行内节点。
;;
;; 语法
;; ----
;; (make-cmark-emoji shortcode)

(let ((node (make-cmark-emoji "smile")))
  (check (cmark-node-type node) => 'emoji)
  (check (assoc 'shortcode (cmark-node-data node)) => '(shortcode . "smile"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试其他短代码
(let ((node (make-cmark-emoji "+1")))
  (check (assoc 'shortcode (cmark-node-data node)) => '(shortcode . "+1"))
) ;let

;; 测试空短代码
(let ((node (make-cmark-emoji "")))
  (check (assoc 'shortcode (cmark-node-data node)) => '(shortcode . ""))
) ;let

(check-report)
