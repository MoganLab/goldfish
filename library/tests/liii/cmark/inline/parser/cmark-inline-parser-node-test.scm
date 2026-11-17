;; cmark-inline-parser-node
;; 获取内联解析器的关联节点。
;;
;; 语法
;; ----
;; (cmark-inline-parser-node parser)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;;
;; 返回值
;; ----
;; any
;; 返回 node 字段。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (check (cmark-inline-parser-node p) => #f)
) ;let

(check-report)
