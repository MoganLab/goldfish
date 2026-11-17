
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-inline-html
;; 解析行内 HTML 片段。
;;
;; 语法
;; ----
;; (parse-inline-html str start)
;;
;; 描述
;; ----
;; inline-html-start? 用于判断当前位置是否可能是 HTML 标签。

(check-true (inline-html-start? "<span>text</span>" 0))
(check-true (inline-html-start? "</span>" 0))
(check-true (inline-html-start? "<!-- note -->" 0))
(check-true (inline-html-start? "<!DOCTYPE html>" 0))
(check-true (inline-html-start? "<?php echo 1; ?>" 0))
(check-true (inline-html-start? "<![CDATA[x]]>" 0))
(check-false (inline-html-start? "text" 0))
(check-false (inline-html-start? "<123>" 0))

(let ((result (parse-inline-html "<span>" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check (cdr result) => 6)
) ;let

(let ((result (parse-inline-html "<unclosed" 0)))
  ;; 未闭合 HTML：只消费起始 < 作为文本，剩余由 parse-inlines 继续调度
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(let ((result (parse-inline-html "</span>" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "</span>"
  ) ;check
  (check (cdr result) => 7)
) ;let

(let ((result (parse-inline-html "<!-- note -->" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "<!-- note -->"
  ) ;check
  (check (cdr result) => 13)
) ;let

(let ((result (parse-inline-html "<span data-text=\"a > b\">" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "<span data-text=\"a > b\">"
  ) ;check
) ;let

(let ((result (parse-inline-html "<span class='pill'>" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "<span class='pill'>"
  ) ;check
) ;let

(let ((result (parse-inline-html "<span data-x=1>" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "<span data-x=1>"
  ) ;check
) ;let

(let ((result (parse-inline-html "<span\nclass=\"pill\">" 0)))
  (check (cmark-node-type (car result)) => 'html_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "<span\nclass=\"pill\">"
  ) ;check
) ;let

(let ((result (parse-inline-html "</span class=\"x\">" 0)))
  ;; closing tag 后带 attribute 非法：只消费起始 < 作为文本，剩余交还 parse-inlines
  (check (cmark-node-type (car result)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car result))))
    =>
    "<"
  ) ;check
) ;let

(let ((result (parse-inline-html "<!-- note" 0)))
  ;; 未闭合注释：只消费起始 < 作为文本，剩余交还 parse-inlines
  (check (cmark-node-type (car result)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car result))))
    =>
    "<"
  ) ;check
) ;let

(check-report)
