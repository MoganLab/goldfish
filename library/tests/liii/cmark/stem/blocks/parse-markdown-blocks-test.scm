;; parse-markdown-blocks
;; 将 Markdown 行列表解析为 md-stem 块级节点列表。
;;
;; 语法
;; ----
;; (parse-markdown-blocks lines . maybe-refmap)
;;
;; 参数
;; ----
;; lines : list
;; Markdown 行字符串列表。
;;
;; 返回值
;; ----
;; list
;; 返回 md-stem 块节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark stem blocks))

(check-set-mode! 'report-failed)

(check (parse-markdown-blocks '("# Hi" "" "hello")) => '((h1 "Hi") (p "hello")))

(check-report)
