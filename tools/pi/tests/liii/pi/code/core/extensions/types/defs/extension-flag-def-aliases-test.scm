;; extension-flag-def-aliases
;; 读取标志定义中的别名列表。
;;
;; 语法
;; ----
;; (extension-flag-def-aliases f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 别名列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-aliases f) => '()))

(check-report)
