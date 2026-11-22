;; parse-strikeout
;; 解析 ~~ 包裹的删除线内容。
;;
;; 语法
;; ----
;; (parse-strikeout str start)
;;
;; 参数
;; ----
;; str : any
;; 输入参数。
;; start : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。

(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (strikeout-start? "~~gone~~" 0))
(check-false (strikeout-start? "~gone~" 0))
(check-false (strikeout-start? "~~~gone~~~" 0))

(let ((result (parse-strikeout "~~hello~~" 0)))
  (check (cmark-node-type (car result)) => 'strikeout)
  (check (cdr result) => 9)
) ;let

(let ((result (parse-strikeout "~~`code`~~" 0)))
  (check (cmark-node-type (car result)) => 'strikeout)
  (check
    (cmark-node-type (car (cmark-node-children (car result))))
    =>
    'code
  ) ;check
) ;let

(let ((result (parse-strikeout "~~[doc](https://example.com)~~" 0)))
  (check (cmark-node-type (car result)) => 'strikeout)
  (check
    (cmark-node-type (car (cmark-node-children (car result))))
    =>
    'link
  ) ;check
) ;let

(let ((result (parse-strikeout "~~unclosed" 0)))
  ;; 未闭合：只消费起始 ~~ 作为文本，剩余由 parse-inlines 继续调度
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 2)
) ;let

(check-report)
