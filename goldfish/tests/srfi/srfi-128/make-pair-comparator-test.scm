;; make-pair-comparator
;; 为二元组创建比较器。
;;
;; 语法
;; ----
;; (make-pair-comparator car-comparator cdr-comparator)
;;
;; 参数
;; ----
;; car-comparator : comparator
;; 用于比较 car 部分的比较器。
;; cdr-comparator : comparator
;; 用于比较 cdr 部分的比较器。
;; 返回值
;; ----
;; comparator
;; 返回用于比较二元组的比较器。
;; 注意
;; ----
;; 先比较 car 部分，car 相等时再比较 cdr 部分。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((num (make-comparator number? = < number-hash))
       (lst (make-list-comparator num list? null? car cdr))
       (comp (make-pair-comparator num lst)))
  (check-true (comparator? comp))
  (check (=? comp '(1 2) '(1 2)) => #t)
  (check (=? comp '(1 2) '(1 3)) => #f)
  (check (<? comp '(1 2) '(1 3)) => #t)
  (check (<? comp '(1 2) '(2 1)) => #t)
  (check (<? comp '(1 2) '(1 3 4)) => #t)
  (check (comparator-test-type comp '(1 2)) => #t)
  (check (comparator-test-type comp '(1 "a")) => #f)
) ;let*

(check-report)
