(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-paragraph
;; 把若干普通文本行组合为段落节点。
;;
;; 语法
;; ----
;; (parse-paragraph lines)
;;
;; 描述
;; ----
;; blank-line? 与 paragraph-continues? 常用于决定段落是否开始或继续。

(check-true (blank-line? ""))
(check-false (blank-line? "Hello"))
(check-true (paragraph-continues? "Hello"))
(check-false (paragraph-continues? "# Heading"))
(check-false (paragraph-continues? ""))

(check (cmark-node-type (parse-paragraph '("Hello world"))) => 'paragraph)

(let ((paragraph (parse-paragraph '("Line 1" "Line 2"))))
  (check (cdr (assoc 'content (cmark-node-data paragraph))) => "Line 1\nLine 2")
) ;let

(check-report)
