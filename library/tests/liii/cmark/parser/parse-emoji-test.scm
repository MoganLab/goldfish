(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-emoji
;; 解析 `:smile:` 这类短代码表情。
;;
;; 语法
;; ----
;; (parse-emoji str start)

(check-true (emoji-start? ":smile:" 0))
(check-true (emoji-start? ":rocket:" 0))
(check-false (emoji-start? ":unknown:" 0))
(check-false (emoji-start? "::" 0))

(let ((result (parse-emoji ":smile:" 0)))
  (check (cmark-node-type (car result)) => 'emoji)
  (check (assoc 'shortcode (cmark-node-data (car result)))
    =>
    '(shortcode . "smile")
  ) ;check
  (check (cdr result) => 7)
) ;let

(let ((result (parse-emoji ":+1:" 0)))
  (check (cmark-node-type (car result)) => 'emoji)
  (check (assoc 'shortcode (cmark-node-data (car result))) => '(shortcode
                                                                 . "+1"))
) ;let

(check-report)
