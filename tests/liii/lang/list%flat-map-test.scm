;; list%flat-map
;; 映射后展平一层（对标 Scala List.flatMap）。
;;
;; 语法
;; ----
;; ($ xs :flat-map f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参函数，返回集合（list/vector/string）；返回单值视为单元素。
;;
;; 返回值
;; ------
;; list?
;; 各结果按序拼接展平一层。
;;
;;   => ($ '(1 2 3) :flat-map (lambda (x) (list x (* x 10))))  ; (1 10 2 20 3 30)

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check
  ($ '(1 2 3) :flat-map (lambda (x) (list x (* x 10))))
  =>
  '(1 10 2 20 3 30)
) ;check
(check ($ '() :flat-map (lambda (x) (list x))) => '())
(check ($ '(1 2) :flat-map (lambda (x) (list x))) => '(1 2))
(check ($ '(1 2) :flat-map (lambda (x) x)) => '(1 2))
(check ($ '(1 2) :flat-map (lambda (x) (vector x x))) => '(1 1 2 2))

(check-report)
