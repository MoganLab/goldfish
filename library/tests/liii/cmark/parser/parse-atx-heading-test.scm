
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-atx-heading
;; 解析以 # 开头的 ATX 标题行。
;;
;; 语法
;; ----
;; (parse-atx-heading line)
;;
;; 描述
;; ----
;; 配套的 atx-heading-line? 用于判断一行是否满足 ATX 标题入口。

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
