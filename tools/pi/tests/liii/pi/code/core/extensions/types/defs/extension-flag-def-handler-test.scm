;; extension-flag-def-handler
;; 读取标志定义中的处理过程。
;;
;; 语法
;; ----
;; (extension-flag-def-handler f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 处理过程。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-handler f) => #f)
) ;let

(check-report)
