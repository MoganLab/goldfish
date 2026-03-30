(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-root
;; 返回根路径。
;;
;; 语法
;; ----
;; (path-root)
;;
;; 返回值
;; ----
;; path-value
;; 返回根路径 "/"。
;;
;; 示例
;; ----
;; (path->string (path-root)) => "/"

(check (path->string (path-root)) => "/")
(check (path-type (path-root)) => 'posix)

(when (not (os-windows?))
  (check-true (path-absolute? (path-root)))
) ;when

(check-report)
