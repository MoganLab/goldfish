;; extension-flag-def-type
;; 读取标志定义中的取值类型符号。
;;
;; 语法
;; ----
;; (extension-flag-def-type f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 取值类型符号。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-type f) => 'boolean)
) ;let

(check-report)
