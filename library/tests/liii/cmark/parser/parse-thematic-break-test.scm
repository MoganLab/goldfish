(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-thematic-break
;; 解析 Markdown 分隔线。
;;
;; 语法
;; ----
;; (parse-thematic-break)
;;
;; 描述
;; ----
;; thematic-break-line? 用于判断某一行是否满足分隔线语法。

(check-true (thematic-break-line? "---"))
(check-true (thematic-break-line? "***"))
(check-true (thematic-break-line? "___"))
(check-true (thematic-break-line? "- - -"))
(check-true (thematic-break-line? "  ---"))
(check-false (thematic-break-line? "--"))
(check-false (thematic-break-line? "    ---"))
(check-false (thematic-break-line? "hello"))
(check (cmark-node-type (parse-thematic-break)) => 'thematic_break)

(check-report)
