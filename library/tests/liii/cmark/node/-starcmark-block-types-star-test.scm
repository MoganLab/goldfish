;; *cmark-block-types*
;; 包含全部块级节点类型符号的列表。
;;
;; 语法
;; ----
;; (*cmark-block-types*)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; list
;; 块级节点类型符号列表。

(import (scheme base))
(import (liii check))
(import (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (list? *cmark-block-types*))
(check-true (if (memq 'document *cmark-block-types*) #t #f))
(check-true (if (memq 'paragraph *cmark-block-types*) #t #f))
(check-true (if (memq 'heading *cmark-block-types*) #t #f))

(check-report)
