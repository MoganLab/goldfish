(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path=?
;; 判断两个路径值是否相等。
;;
;; 语法
;; ----
;; (path=? path1 path2)
;;
;; 参数
;; ----
;; path1, path2 : path-value
;; 要比较的两个路径值。
;;
;; 返回值
;; ----
;; boolean
;; 当两个路径值相等时返回 #t，否则返回 #f。

(check-true (path=? (path "tmp/demo.txt") (path-copy (path "tmp/demo.txt"))))

(check-report)
