
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check)
  (liii cmark inline-parser)
  (liii cmark node)
  (liii utils markdown-reference-util)
) ;import

(check-set-mode! 'report-failed)

;; parse-image
;; 解析 Markdown 图片语法。
;;
;; 语法
;; ----
;; (parse-image str start)
;;
;; 描述
;; ----
;; image-start? 用于判断当前位置是否以 ![ 开始。

(check-true (image-start? "![alt](url)" 0))
(check-false (image-start? "[link](url)" 0))
(check-false (image-start? "text" 0))

(let ((result (parse-image "![hello](https://example.com/img.png)" 0)))
  (check (cmark-node-type (car result)) => 'image)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com/img.png"
  ) ;check
  (check (cdr result) => 37)
) ;let

(let ((result (parse-image "![unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  ;; 未闭合 image 只消费起始 !（CommonMark：后续内容继续按行内解析）
  (check (cdr result) => 1)
) ;let

(let ((result (parse-image "![Logo][img]" 0)))
  ;; 无 refmap 时 reference image 不解析；只消费起始 !，剩余由 parse-inlines 继续调度
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "img" "logo.png" "项目 Logo")
  (let* ((result (parse-image "![Logo][img]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'image)
    (check (cdr (assoc 'url (cmark-node-data node))) => "logo.png")
    (check (cdr (assoc 'title (cmark-node-data node))) => "项目 Logo")
    (check (cdr result) => 12)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "logo" "logo.png" "")
  (let* ((result (parse-image "![Logo][]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'image)
    (check (cdr (assoc 'url (cmark-node-data node))) => "logo.png")
    (check (cdr result) => 9)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "logo" "logo.png" "")
  (let* ((result (parse-image "![Logo]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'image)
    (check (cdr (assoc 'url (cmark-node-data node))) => "logo.png")
    (check (cdr result) => 7)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "my&img" "logo.png" "")
  (let* ((result (parse-image "![Logo][my&amp;img]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'image)
    (check (cdr (assoc 'url (cmark-node-data node))) => "logo.png")
  ) ;let*
) ;let

;; 测试带双引号 title 的图片
(let ((result (parse-image "![alt](https://example.com/img.png \"title\")" 0)))
  (check (cmark-node-type (car result)) => 'image)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com/img.png"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    "title"
  ) ;check
  (check (cdr result) => 43)
) ;let

;; 测试带单引号 title 的图片
(let ((result (parse-image "![alt](https://example.com/img.png 'title')" 0)))
  (check (cmark-node-type (car result)) => 'image)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com/img.png"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    "title"
  ) ;check
  (check (cdr result) => 43)
) ;let

;; 测试无 title 的图片（确保不破坏原有功能）
(let ((result (parse-image "![alt](https://example.com/img.png)" 0)))
  (check (cmark-node-type (car result)) => 'image)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com/img.png"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    ""
  ) ;check
  (check (cdr result) => 35)
) ;let

;; 测试空的 alt 文本
(let ((result (parse-image "![](url.png)" 0)))
  (check (cmark-node-type (car result)) => 'image)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "url.png"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    ""
  ) ;check
  (check (cdr result) => 12)
) ;let

;; 图片 alt、URL 和 title 中的实体应被解码
(let* ((result (parse-image "![A &amp; B](https://example.com?a=1&amp;b=2 \"T &amp; T\")" 0)
       ) ;result
       (node (car result))
      ) ;
  (check (cmark-node-type node) => 'image)
  (check (cdr (assoc 'url (cmark-node-data node)))
    =>
    "https://example.com?a=1&b=2"
  ) ;check
  (check (cdr (assoc 'title (cmark-node-data node))) => "T & T")
  (check (length (cmark-node-children node)) => 1)
  (check
    (cdr
      (assoc 'content (cmark-node-data (car (cmark-node-children node))))
    ) ;cdr
    =>
    "A & B"
  ) ;check
) ;let*

(check-report)
