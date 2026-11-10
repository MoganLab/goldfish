;; extension-flag-def-description
;; 读取标志定义中的描述字符串。
;;
;; 语法
;; ----
;; (extension-flag-def-description f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 描述字符串。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-description f) => "desc")
) ;let

(check-report)
