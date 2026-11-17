;; parse-backslash-escape
;; 解析反斜杠转义并返回 (text-node . next-pos)。
;;
;; 语法
;; ----
;; (parse-backslash-escape str start)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;;
;; 返回值
;; ----
;; pair
;; 返回包含 text 节点与下一位置的点对。

(import (scheme base))
(import (liii check))
(import (liii cmark inline escape))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(let ((res (parse-backslash-escape "\\*a" 0)))
  (check (cmark-node-type (car res)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car res))))
    =>
    "*"
  ) ;check
  (check (cdr res) => 2)
) ;let

(check-report)
