;; extension-flag-def-default
;; 读取标志定义中的默认值。
;;
;; 语法
;; ----
;; (extension-flag-def-default f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 默认值。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-default f) => #f)
) ;let

(check-report)
