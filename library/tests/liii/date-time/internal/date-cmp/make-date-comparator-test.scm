;; make-date-comparator
;; 基于三值比较构造日期比较器。
;;
;; 语法
;; ----
;; (make-date-comparator pred)
;;
;; 参数
;; ----
;; pred : procedure? 对 -1/0/1 比较结果判定的谓词。
;;
;; 返回值
;; ----
;; procedure? 接受两个及以上 date 参数、返回 boolean? 的比较器。

(import (liii check) (liii date-time date) (liii date-time internal date-cmp))

(check-set-mode! 'report-failed)

(define <? (make-date-comparator (lambda (r) (< r 0))))

(define =? (make-date-comparator (lambda (r) (= r 0))))

(check-true (<? (make-date 2021 1 1) (make-date 2021 1 2)))
(check-false (<? (make-date 2021 1 2) (make-date 2021 1 1)))
(check-true (=? (make-date 2021 1 1) (make-date 2021 1 1)))
(check-true (<? (make-date 2020 1 1) (make-date 2021 1 1) (make-date 2022 1 1)))
(check-false (<? (make-date 2020 1 1) (make-date 2022 1 1) (make-date 2021 1 1))
) ;check-false
(check-catch 'type-error (<? (make-date 2021 1 1) "date"))

(check-report)
