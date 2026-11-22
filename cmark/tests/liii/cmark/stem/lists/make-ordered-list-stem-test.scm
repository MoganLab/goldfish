;; make-ordered-list-stem
;; 构造有序列表 ordered-list md-stem 节点。
;;
;; 语法
;; ----
;; (make-ordered-list-stem start delimiter tightness item-lines-list parse-blocks)
;;
;; 参数
;; ----
;; start : integer
;; 起始编号。
;; delimiter : string
;; 分隔符。
;; tightness : symbol
;; 松紧标记。
;; item-lines-list : list
;; 列表项行列表。
;; parse-blocks : procedure
;; 块级解析回调。
;;
;; 返回值
;; ----
;; pair
;; 返回 (ordered-list ...) 节点。

(import (scheme base))
(import (liii check))
(import (liii cmark stem lists))

(check-set-mode! 'report-failed)

(check
  (make-ordered-list-stem 1 "." 'tight '(("a")) (lambda (ls) (list (cons 'p ls))))
  =>
  '(ordered-list 1 "." tight (item (p "a")))
) ;check

(check-report)
