;; extension-flag-def-takes-arg?
;; 读取标志定义中的是否需要参数。
;;
;; 语法
;; ----
;; (extension-flag-def-takes-arg? f)
;;
;; 参数
;; ----
;; f : extension-flag-def
;; 标志定义记录。
;;
;; 返回值
;; ----
;; any
;; 是否需要参数。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((f (make-extension-flag-def "--my-flag" "desc")))
  (check (extension-flag-def-takes-arg? f) => #f)
) ;let

(check-report)
