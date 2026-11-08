;; list%zip-with-index
;; 将每个元素与从 0 开始的下标配对（对标 Scala List.zipWithIndex）。
;;
;; 语法
;; ----
;; ($ xs :zip-with-index)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; ((elem 0) (elem 1) ...) 二元组链表；空表返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '("a" "b" "c") :zip-with-index) => '(("a" 0) ("b" 1) ("c" 2)))
(check ($ '(10 20) :zip-with-index) => '((10 0) (20 1)))
(check ($ '() :zip-with-index) => '())
(check
  ($ '(10 20 30) :zip-with-index :map (lambda (p) (+ (car p) (cadr p))))
  =>
  '(10 21 32)
) ;check

(check-report)
