;; path-home
;; 返回用户主目录路径。
;;
;; 语法
;; ----
;; (path-home)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; path-value
;; 返回用户主目录的路径值。
;;
;; (path->string (path-home)) => (getenv "HOME")
;; (path-absolute? (path-home)) => #t

(import (liii check) (liii path) (liii os) (liii base))

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check (path->string (path-home)) => (getenv "HOME"))
) ;when

(check-true (path-absolute? (path-home)))

(when (os-windows?)
  (check-true (path-exists? (path-home)))
) ;when

(check-report)
