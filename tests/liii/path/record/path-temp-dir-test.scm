;; path-temp-dir
;; 返回系统临时目录路径。
;;
;; 语法
;; ----
;; (path-temp-dir)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; path-value
;; 返回系统临时目录的路径值。
;;
;; (path->string (path-temp-dir)) => (os-temp-dir)
;; (path-absolute? (path-temp-dir)) => #t
;; (path-dir? (path-temp-dir)) => #t

(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

(check (path->string (path-temp-dir)) => (os-temp-dir))
(check-true (path-absolute? (path-temp-dir)))
(check-true (path-dir? (path-temp-dir)))

(check-report)
