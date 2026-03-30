(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-absolute?
;; 判断路径是否为绝对路径。
;;
;; 语法
;; ----
;; (path-absolute? path-value)
;;
;; 参数
;; ----
;; path-value : path-value | string?
;; 要检查的路径。
;;
;; 返回值
;; ----
;; boolean
;; 当路径为绝对路径时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 只根据路径值自身的结构判断，不要求路径实际存在。

;; 基本绝对/相对路径测试
(check-false (path-absolute? (path)))
(check-false (path-absolute? (path "")))
(check-false (path-absolute? (path "relative.txt")))
(check-true (path-absolute? (path-of-drive #\C)))

(when (not (os-windows?))
  (check-true (path-absolute? (path-root)))
  (check-true (path-absolute? (path-join (path-root) "tmp")))
  (check-true (path-absolute? (path "/tmp/demo.txt")))
) ;when

(check-true (path-absolute? (path-home)))
(check-true (path-absolute? (path-temp-dir)))
(check-true (path-absolute? (path-cwd)))

(check-report)
