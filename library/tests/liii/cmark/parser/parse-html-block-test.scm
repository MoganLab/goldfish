
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check)
  (liii cmark block parser)
  (liii cmark node)
  (liii cmark block html)
) ;import

(check-set-mode! 'report-failed)

;; parse-html-block
;; 解析块级 HTML 内容。
;;
;; 语法
;; ----
;; (parse-html-block lines)
;;
;; 描述
;; ----
;; html-block-start? 用于判断某一行是否可作为 HTML 块起始。

(check-true (html-block-start? "<div>"))
(check-true (html-block-start? "<p>"))
(check-true (html-block-start? "<!DOCTYPE html>"))
(check-true (html-block-start? "<!-- note -->"))
(check-true (html-block-start? "<?php"))
(check-true (html-block-start? "<![CDATA["))
(check-true (html-block-start? "<script>"))
(check-true (html-block-start? "  <table>"))
(check-true (html-block-start? "<custom-widget>"))
(check-true (html-block-start? "<span>"))
(check-true (html-block-start? "</div>"))
(check-true (html-block-start? "</div   >"))
(check-true (html-block-start? "<widget data-x=\"1\">"))
(check-true (html-block-start? "<widget />"))
(check-true (html-block-start? "</widget>"))
(check-true (html-block-start? "   <div>"))
(check-true (html-block-start? "   <widget-box>"))
(check-true (html-block-start? "<DIV class=\"x\">"))
(check-true (html-block-start? "<Widget-Box data-x=\"1\">"))
(check-true (html-block-start? (string-append "<widget-box>" "   ")))
(check-true (html-block-start? (string-append "</widget-box>" "   ")))
(check-true (html-block-start? (string-append "</div>" "   ")))
(check-false (html-block-start? "Hello"))
(check-false (html-block-start? "    <widget-box>"))
(check-false (html-block-start? "<https://example.com>"))
(check-false (html-block-start? "<user@example.com>"))
(check-false (html-block-start? "</div class=\"x\">"))
(check-false (html-block-start? "<widget> tail"))

(check-false (html-block-interrupts-paragraph? "<custom-widget>"))
(check-false (html-block-interrupts-paragraph? "<span>"))
(check-false (html-block-interrupts-paragraph? "<widget />"))
(check-false (html-block-interrupts-paragraph? "   <widget-box>"))
(check-false (html-block-interrupts-paragraph? (string-append "<widget-box>" "   "))
) ;check-false
(check-true (html-block-interrupts-paragraph? "<div>"))
(check-true (html-block-interrupts-paragraph? "</div>"))

(let ((html (parse-html-block '("<div>" "  <p>Text</p>" "</div>"))))
  (check (cmark-node-type html) => 'html_block)
  (check (cdr (assoc 'literal (cmark-node-data html)))
    =>
    "<div>\n  <p>Text</p>\n</div>"
  ) ;check
) ;let

(let ((html (parse-html-block '("<!--" "comment" "-->"))))
  (check (cmark-node-type html) => 'html_block)
  (check (cdr (assoc 'literal (cmark-node-data html))) => "<!--\ncomment\n-->")
) ;let

(let ((html (parse-html-block '("<script>" "" "console.log(1);" "</script>"))))
  (check (cmark-node-type html) => 'html_block)
  (check (cdr (assoc 'literal (cmark-node-data html)))
    =>
    "<script>\n\nconsole.log(1);\n</script>"
  ) ;check
) ;let

(let* ((start-line (string-append "<widget-box>" "   "))
       (parsed (collect-html-block-lines (list start-line "body line" "" "tail")))
      ) ;
  (check (car parsed) => (list start-line "body line"))
  (check (cdr parsed) => '("" "tail"))
) ;let*

(check-report)
