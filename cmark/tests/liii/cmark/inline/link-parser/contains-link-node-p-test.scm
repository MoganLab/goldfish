;; contains-link-node?
;; 检查节点树列表中是否包含 link 节点。
;;
;; 语法
;; ----
;; (contains-link-node? nodes)
;;
;; 参数
;; ----
;; nodes : list
;; cmark-node 列表。
;;
;; 返回值
;; ----
;; boolean
;; 包含 link 节点返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link-parser))
(import (liii cmark node))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(check-true (contains-link-node? (list (make-cmark-link "u" ""))))
(check-false (contains-link-node? (list (make-cmark-text "t"))))

(check-report)
