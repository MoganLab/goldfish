(import (liii check)
        (liii path)
        (liii os)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

;; path-home
;; 返回用户主目录路径。
;;
;; 语法
;; ----
;; (path-home)
;;
;; 返回值
;; ----
;; path-value
;; 返回用户主目录的路径值。
;;
;; 示例
;; ----
;; (path->string (path-home)) => (getenv "HOME")
;; (path-absolute? (path-home)) => #t

(when (not (os-windows?))
  (check (path->string (path-home)) => (getenv "HOME"))
) ;when

(check-true (path-absolute? (path-home)))

(when (os-windows?)
  (check-true (path-exists? (path-home)))
) ;when

(check-report)
