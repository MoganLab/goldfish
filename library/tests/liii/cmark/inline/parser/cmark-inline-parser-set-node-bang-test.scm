;; cmark-inline-parser-set-node!
;; 设置内联解析器的关联节点。
;;
;; 语法
;; ----
;; (cmark-inline-parser-set-node! parser node)
;;
;; 参数
;; ----
;; parser : cmark-inline-parser
;; 解析器实例。
;; node : any
;; 关联节点。
;;
;; 返回值
;; ----
;; unspecified
;; 原地更新 node。

(import (scheme base))
(import (liii check))
(import (liii cmark inline parser))

(check-set-mode! 'report-failed)

(let ((p (cmark-inline-parser-new)))
  (cmark-inline-parser-set-node! p 'node)
  (check (cmark-inline-parser-node p) => 'node)
) ;let

(check-report)
