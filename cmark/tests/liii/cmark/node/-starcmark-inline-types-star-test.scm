;; *cmark-inline-types*
;; 包含全部行内节点类型符号的列表。
;;
;; 语法
;; ----
;; (*cmark-inline-types*)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; list
;; 行内节点类型符号列表。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (list? *cmark-inline-types*))
(check-true (if (memq 'text *cmark-inline-types*) #t #f))
(check-true (if (memq 'emph *cmark-inline-types*) #t #f))
(check-true (if (memq 'strong *cmark-inline-types*) #t #f))

(check-report)
