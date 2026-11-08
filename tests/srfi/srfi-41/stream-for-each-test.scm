;; stream-for-each
;; 对多个流同步逐元素应用过程，丢弃返回值。
;;
;; 语法
;; ----
;; (stream-for-each proc strm ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受每个流的一个元素。
;;
;; strm : stream?
;; 一个或多个流。
;; 返回值
;; ----
;; 未指定（副作用过程）。
;;
;; 说明
;; ----
;; 错误：proc 非过程/无流参数/非流参数分别报错。
;; 任一流到尽头即停止。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-for-each (stream-for-each "four" (stream 1)))
(check-catch 'stream-for-each (stream-for-each display))
(check-catch 'stream-for-each (stream-for-each display "four"))
(check (let ((sum 0))
          (stream-for-each (lambda (x) (set! sum (+ sum x))) (stream 1 2 3))
          sum)
  => 6)
(check (let ((pairs '()))
          (stream-for-each
            (lambda (a b) (set! pairs (cons (list a b) pairs)))
            (stream 1 2 3) (stream 10 20 30))
          (reverse pairs))
  => '((1 10) (2 20) (3 30)))

(check-report)
