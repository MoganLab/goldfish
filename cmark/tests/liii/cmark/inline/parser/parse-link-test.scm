;; parse-link
;; 解析 Markdown 链接语法。
;;
;; 语法
;; ----
;; (parse-link str start)
;;
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;; start : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; link-start? 用于判断当前位置是否以 [ 开始。

(import (liii check)
  (liii cmark inline parser)
  (liii cmark node)
  (liii cmark block reference)
) ;import

(check-set-mode! 'report-failed)

(check-true (link-start? "[link](url)" 0))
(check-false (link-start? "text" 0))
(check-true (link-start? "a[b]" 1))

(let ((result (parse-link "[hello](https://example.com)" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check (cdr result) => 28)
) ;let

(let ((result (parse-link "[hello][ref]" 0)))
  ;; 无 refmap 时 reference link 不解析；只消费起始 [，剩余由 parse-inlines 继续调度
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "ref" "https://example.com/ref" "")
  (let* ((result (parse-link "[hello][ref]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'link)
    (check (cdr (assoc 'url (cmark-node-data node))) => "https://example.com/ref")
    (check (cdr result) => 12)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "hello" "https://example.com/collapsed"
    ""
  ) ;reference-map-add-definition!
  (let* ((result (parse-link "[hello][]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'link)
    (check (cdr (assoc 'url (cmark-node-data node)))
      =>
      "https://example.com/collapsed"
    ) ;check
    (check (cdr result) => 9)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "hello" "https://example.com/shortcut" "")
  (let* ((result (parse-link "[hello]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'link)
    (check (cdr (assoc 'url (cmark-node-data node)))
      =>
      "https://example.com/shortcut"
    ) ;check
    (check (cdr result) => 7)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "my label" "https://example.com/label" "")
  (let* ((result (parse-link "[hello][My   Label]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'link)
    (check (cdr (assoc 'url (cmark-node-data node))) => "https://example.com/label")
    (check (cdr result) => 19)
  ) ;let*
) ;let

(let ((refmap (make-reference-map)))
  (reference-map-add-definition! refmap "my&ref" "https://example.com/entity" "")
  (let* ((result (parse-link "[hello][my&amp;ref]" 0 refmap)) (node (car result)))
    (check (cmark-node-type node) => 'link)
    (check (cdr (assoc 'url (cmark-node-data node)))
      =>
      "https://example.com/entity"
    ) ;check
  ) ;let*
) ;let

(let* ((parsed (collect-link-reference-definition '("[doc]: https://example.com/doc"
                                                    "  \"Doc Title\""
                                                    "next")
               ) ;collect-link-reference-definition
       ) ;parsed
       (definition (car parsed))
      ) ;
  (check (car definition) => "doc")
  (check (car (cdr definition)) => "https://example.com/doc")
  (check (cdr (cdr definition)) => "Doc Title")
  (check (cdr parsed) => '("next"))
) ;let*

(let ((result (parse-link "[unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  ;; 未闭合 link 只消费起始 [（CommonMark：后续内容继续按行内解析，Example 342）
  (check (cdr result) => 1)
) ;let

;; 测试带双引号 title 的链接
(let ((result (parse-link "[text](https://example.com \"title\")" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    "title"
  ) ;check
  (check (cdr result) => 35)
) ;let

;; 测试带单引号 title 的链接
(let ((result (parse-link "[text](https://example.com 'title')" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    "title"
  ) ;check
  (check (cdr result) => 35)
) ;let

;; 测试带括号 title 的链接（简化版，无嵌套括号问题）
(let ((result (parse-link "[text](url (title))" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "url"
  ) ;check
  (check (cdr result) => 19)
) ;let

;; 测试无 title 的链接（确保不破坏原有功能）
(let ((result (parse-link "[text](https://example.com)" 0)))
  (check (cmark-node-type (car result)) => 'link)
  (check
    (cdr (assoc 'url (cmark-node-data (car result))))
    =>
    "https://example.com"
  ) ;check
  (check
    (cdr (assoc 'title (cmark-node-data (car result))))
    =>
    ""
  ) ;check
  (check (cdr result) => 27)
) ;let

;; 链接文本、URL 和 title 中的实体应被解码
(let* ((result (parse-link "[A &amp; B](https://example.com?a=1&amp;b=2 \"T &amp; T\")" 0)
       ) ;result
       (node (car result))
      ) ;
  (check (cmark-node-type node) => 'link)
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
