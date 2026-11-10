;; path-cwd
;; 返回当前工作目录路径。
;;
;; 语法
;; ----
;; (path-cwd)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; path-value
;; 返回当前工作目录的路径值。
;;
;; (path->string (path-cwd)) => (getcwd)
;; (path-absolute? (path-cwd)) => #t

(import (liii check) (liii path) (liii os) (liii base))

(check-set-mode! 'report-failed)

(check (path->string (path-cwd)) => (getcwd))
(check-true (path-absolute? (path-cwd)))

(check-report)
