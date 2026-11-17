;; parse-list-marker
;; 解析列表项标记行，返回信息或 #f。
;;
;; 语法
;; ----
;; (parse-list-marker line)
;;
;; 参数
;; ----
;; line : string
;; 输入行。
;;
;; 返回值
;; ----
;; list 或 #f
;; 合法列表行返回 (type start delim body content-indent)，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))

(check-set-mode! 'report-failed)

(let ((m (parse-list-marker "- item")))
  (check-true (markdown-list-marker? m))
  (check (markdown-list-marker-type m) => 'bullet)
) ;let
(check (parse-list-marker "plain") => #f)

(check-report)
