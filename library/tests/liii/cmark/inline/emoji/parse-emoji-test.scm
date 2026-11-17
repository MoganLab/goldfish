;; parse-emoji
;; 解析 `:smile:` 这类短代码表情。
;;
;; 语法
;; ----
;; (parse-emoji str start)
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

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

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
