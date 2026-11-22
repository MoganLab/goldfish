;; parse-list-item
;; 将列表项行解析为 item cmark-node。
;;
;; 语法
;; ----
;; (parse-list-item line)
;;
;; 参数
;; ----
;; line : string
;; 列表项行。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回 item 节点。

(import (scheme base))
(import (liii check))
(import (liii cmark block list))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check (parse-list-item "- hi") => '(bullet . "hi"))

(check-report)
