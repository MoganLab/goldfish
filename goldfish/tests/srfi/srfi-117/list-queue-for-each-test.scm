;; list-queue-for-each
;; 从队首到队尾依次对每个元素调用过程。
;;
;; 语法
;; ----
;; (list-queue-for-each proc list-queue)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一元过程，被依次作用于队列的每个元素。
;;
;; list-queue : list-queue?
;; 待遍历的队列。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未定义。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-for-each 基本测试
(let ((q (list-queue 1 2 3)) (sum 0))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x))) q)
  (check sum => 6)
) ;let


;; 空队列
(let ((q (list-queue)) (count 0))
  (list-queue-for-each (lambda (x) (set! count (+ count 1))) q)
  (check count => 0)
) ;let


;; 单元素队列
(let ((q (list-queue 42)) (val 0))
  (list-queue-for-each (lambda (x) (set! val x)) q)
  (check val => 42)
) ;let


;; 收集元素
(let ((q (list-queue 'a 'b 'c)) (result '()))
  (list-queue-for-each (lambda (x) (set! result (cons x result))) q)
  (check result => '(c b a))
) ;let


(check-report)
