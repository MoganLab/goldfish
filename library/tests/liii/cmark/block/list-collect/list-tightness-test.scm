;; list-tightness
;; 判定列表的松紧度（tight 或 loose）。
;;
;; 语法
;; ----
;; (list-tightness item-lines-list content-indents)
;;
;; 参数
;; ----
;; item-lines-list : list
;; 列表项行集合。
;; content-indents : list
;; 各列表项内容缩进。
;;
;; 返回值
;; ----
;; symbol
;; 返回 tight 或 loose。

(import (scheme base))
(import (liii check))
(import (liii cmark block list-collect))

(check-set-mode! 'report-failed)

(check (list-tightness '(("a") ("b")) '(2 2)) => 'tight)
(check (list-tightness '(("a" "" "b")) '(2)) => 'loose)

(check-report)
