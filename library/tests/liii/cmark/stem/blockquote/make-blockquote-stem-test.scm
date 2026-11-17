;; make-blockquote-stem
;; 将引用块行列表构造为 blockquote md-stem 节点。
;;
;; 语法
;; ----
;; (make-blockquote-stem lines parse-blocks)
;;
;; 参数
;; ----
;; lines : list
;; 引用块原始行列表。
;; parse-blocks : procedure
;; 块级解析回调。
;;
;; 返回值
;; ----
;; pair
;; 返回 (blockquote ...) 节点。

(import (scheme base))
(import (liii check))
(import (liii cmark stem blockquote))

(check-set-mode! 'report-failed)

(check
  (make-blockquote-stem '("> a" "> b") (lambda (ls) (list (cons 'p ls))))
  =>
  '(blockquote (p "a" "b"))
) ;check

(check-report)
