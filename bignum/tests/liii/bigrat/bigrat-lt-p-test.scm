;; bigrat<?
;; 判断第一个 <bigrat> 是否严格小于第二个 <bigrat>。
;;
;; 语法
;; ----
;; (bigrat<? a b)
;;
;; 参数
;; ----
;; a, b : bigrat? 有理数。
;;
;; 返回值
;; ----
;; boolean? 严格小于返回 #t，否则返回 #f。

(import (liii check) (liii bigrat))

(check-set-mode! 'report-failed)

(check (bigrat<? (make-bigrat 1 3) (make-bigrat 1 2)) => #t)
(check (bigrat<? (make-bigrat 1 2) (make-bigrat 1 3)) => #f)

(check-report)
