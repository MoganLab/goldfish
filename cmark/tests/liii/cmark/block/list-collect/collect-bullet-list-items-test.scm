;; collect-bullet-list-items
;; 收集连续无序列表项行集合。
;;
;; 语法
;; ----
;; (collect-bullet-list-items lines)
;;
;; 参数
;; ----
;; lines : list
;; 行字符串列表。
;;
;; 返回值
;; ----
;; pair
;; 返回收集结果。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(let ((res (collect-bullet-list-items '("- a" "- b" "" "# after"))))
  (check (cdr res) => '("# after"))
) ;let

(check-report)
