;; extension-flag-def-flag
;; 读取标志定义中的命令行标志字符串。
;;
;; 语法
;; ----
;; (extension-flag-def-flag f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 命令行标志字符串。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-flag f) => "--my-flag"))

(check-report)
