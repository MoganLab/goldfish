(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-equals?
;; 判断两个路径是否相等（忽略大小写比较）。
;;
;; 语法
;; ----
;; (path-equals? path1 path2)
;;
;; 参数
;; ----
;; path1, path2 : path-value | string?
;; 要比较的两个路径。
;;
;; 返回值
;; ----
;; boolean
;; 当两个路径相等时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 在 Windows 上路径比较不区分大小写。

(check-true (path-equals? (path-copy (path-of-drive #\d)) (path-of-drive #\D)))
(check-true (path-equals? (path "tmp/demo.txt") "tmp/demo.txt"))

(check-report)
