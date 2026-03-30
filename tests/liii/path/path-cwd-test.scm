(import (liii check)
        (liii path)
        (liii os)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

;; path-cwd
;; 返回当前工作目录路径。
;;
;; 语法
;; ----
;; (path-cwd)
;;
;; 返回值
;; ----
;; path-value
;; 返回当前工作目录的路径值。
;;
;; 示例
;; ----
;; (path->string (path-cwd)) => (getcwd)
;; (path-absolute? (path-cwd)) => #t

(check (path->string (path-cwd)) => (getcwd))
(check-true (path-absolute? (path-cwd)))

(check-report)
