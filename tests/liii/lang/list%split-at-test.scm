;; list%split-at
;; 在下标 n 处将集合二分为两个子集合（对标 Scala List.splitAt）。
;;
;; 语法
;; ----
;; ($ xs :split-at n)
;;
;; 参数
;; ----
;; n : integer?
;; 切割下标。
;;
;; 返回值
;; ------
;; list?
;; ((take n) (drop n)) 二元组，越界宽容。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4 5) :split-at 2) => '((1 2) (3 4 5)))
(check ($ '(1 2 3) :split-at 0) => '(() (1 2 3)))
(check ($ '(1 2 3) :split-at 10) => '((1 2 3) ()))
(check ($ '(1 2 3) :split-at -1) => '(() (1 2 3)))
(check ($ '() :split-at 2) => '(() ()))

(check-report)
