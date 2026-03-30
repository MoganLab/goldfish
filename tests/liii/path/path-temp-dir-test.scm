(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-temp-dir
;; 返回系统临时目录路径。
;;
;; 语法
;; ----
;; (path-temp-dir)
;;
;; 返回值
;; ----
;; path-value
;; 返回系统临时目录的路径值。
;;
;; 示例
;; ----
;; (path->string (path-temp-dir)) => (os-temp-dir)
;; (path-absolute? (path-temp-dir)) => #t
;; (path-dir? (path-temp-dir)) => #t

(check (path->string (path-temp-dir)) => (os-temp-dir))
(check-true (path-absolute? (path-temp-dir)))
(check-true (path-dir? (path-temp-dir)))

(check-report)
