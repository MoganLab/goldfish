;; parse-atx-heading
;; 解析以 # 开头的 ATX 标题行。
;;
;; 语法
;; ----
;; (parse-atx-heading line)
;;
;;
;; 参数
;; ----
;; line : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; 配套的 atx-heading-line? 用于判断一行是否满足 ATX 标题入口。

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (atx-heading-line? "# Hello"))
(check-true (atx-heading-line? "## Hello"))
(check-false (atx-heading-line? "Hello"))
(check-false (atx-heading-line? "\\# Hello"))

(let ((heading (parse-atx-heading "# Hello")))
  (check (cmark-node-type heading) => 'heading)
  (check (cdr (assoc 'level (cmark-node-data heading))) => 1)
) ;let

(let ((heading (parse-atx-heading "### Hello")))
  (check (cdr (assoc 'level (cmark-node-data heading))) => 3)
) ;let

(let ((heading (parse-atx-heading "# Hello ###")))
  (check (cdr (assoc 'level (cmark-node-data heading))) => 1)
) ;let

(check-report)
