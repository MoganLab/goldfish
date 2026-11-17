;; parse-thematic-break
;; 解析 Markdown 分隔线。
;;
;; 语法
;; ----
;; (parse-thematic-break)
;;
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; thematic-break-line? 用于判断某一行是否满足分隔线语法。

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

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
