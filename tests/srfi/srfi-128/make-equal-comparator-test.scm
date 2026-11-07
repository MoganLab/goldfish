;; make-equal-comparator
;; 创建以 equal? 判定相等的比较器。
;;
;; 语法
;; ----
;; (make-equal-comparator)
;;
;; 参数
;; ----

;; 返回值
;; ----
;; comparator
;; 返回使用 equal? 判定相等的比较器。
;; 注意
;; ----
;; 该比较器不支持排序。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-equal-comparator)))
  (check-true (comparator? comp))
  (check-false (comparator-ordered? comp))
  (check (=? comp '(1 2) '(1 2)) => #t)
  (check (=? comp '(1 2) '(1 3)) => #f)
  (check (=? comp "x" "x") => #t)
) ;let

(check-report)
