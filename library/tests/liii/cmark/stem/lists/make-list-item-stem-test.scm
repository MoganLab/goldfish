;; make-list-item-stem
;; 将单个列表项行列表转换为 md-stem item 节点。
;;
;; 语法
;; ----
;; (make-list-item-stem lines parse-blocks)
;;
;; 参数
;; ----
;; lines : list
;; 列表项内容行。
;; parse-blocks : procedure
;; 块级解析回调。
;;
;; 返回值
;; ----
;; pair
;; 返回 (item ...) 节点。

(import (scheme base))
(import (liii check))
(import (liii cmark stem lists))

(check-set-mode! 'report-failed)

(check
  (make-list-item-stem '("hello") (lambda (ls) (list (cons 'p ls))))
  =>
  '(item (p "hello"))
) ;check

(check-report)
