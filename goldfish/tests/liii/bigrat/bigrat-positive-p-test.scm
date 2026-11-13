;; bigrat-positive?
;; 判断 <bigrat> 是否为正数。
;;
;; 语法
;; ----
;; (bigrat-positive? r)
;;
;; 参数
;; ----
;; r : bigrat? 有理数。
;;
;; 返回值
;; ----
;; boolean? 为正数返回 #t，否则返回 #f。

(import (liii check) (liii bigrat))

(check-set-mode! 'report-failed)

(check (bigrat-positive? (make-bigrat 1 3)) => #t)
(check (bigrat-positive? (make-bigrat -1 3)) => #f)

(check-report)
