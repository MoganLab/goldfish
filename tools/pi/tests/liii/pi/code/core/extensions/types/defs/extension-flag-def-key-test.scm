;; extension-flag-def-key
;; 读取标志定义中的配置键符号。
;;
;; 语法
;; ----
;; (extension-flag-def-key f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 配置键符号。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-key f) => 'my-flag))

(check-report)
