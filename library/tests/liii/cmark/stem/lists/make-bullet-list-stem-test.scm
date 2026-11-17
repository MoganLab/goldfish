;; make-bullet-list-stem
;; 构造无序列表 bullet-list md-stem 节点。
;;
;; 语法
;; ----
;; (make-bullet-list-stem tightness item-lines-list parse-blocks)
;;
;; 参数
;; ----
;; tightness : symbol
;; 列表松紧标记。
;; item-lines-list : list
;; 各列表项行列表。
;; parse-blocks : procedure
;; 块级解析回调。
;;
;; 返回值
;; ----
;; pair
;; 返回 (bullet-list ...) 节点。

(import (scheme base))
(import (liii check))
(import (liii cmark stem lists))

(check-set-mode! 'report-failed)

(check
  (make-bullet-list-stem 'tight '(("a") ("b")) (lambda (ls) (list (cons 'p ls))))
  =>
  '(bullet-list tight (item (p "a")) (item (p "b")))
) ;check

(check-report)
