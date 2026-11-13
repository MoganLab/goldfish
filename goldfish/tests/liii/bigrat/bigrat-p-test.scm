;; bigrat?
;; 判断对象是否为 <bigrat> 有理数。
;;
;; 语法
;; ----
;; (bigrat? x)
;;
;; 参数
;; ----
;; x : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是 <bigrat> 返回 #t，否则返回 #f。

(import (liii check) (liii bigrat) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigrat? (make-bigrat (string->bigint "10") (string->bigint "3"))) => #t)
(check (bigrat? 1/3) => #f)
(check (bigrat? 42) => #f)

(check-report)
