;; bigrat=?
;; 判断两个 <bigrat> 是否相等。
;;
;; 语法
;; ----
;; (bigrat=? a b)
;;
;; 参数
;; ----
;; a, b : bigrat? 有理数。
;;
;; 返回值
;; ----
;; boolean? 相等返回 #t，否则返回 #f。

(import (liii check) (liii bigrat))

(check-set-mode! 'report-failed)

(check (bigrat=? (make-bigrat 2 6) (make-bigrat 1 3)) => #t)
(check (bigrat=? (make-bigrat 1 2) (make-bigrat 1 3)) => #f)

(check-report)
