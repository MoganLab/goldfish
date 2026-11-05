;; list-queue-map
;; 用一元过程逐个映射队列元素，返回新的队列。
;;
;; 语法
;; ----
;; (list-queue-map proc list-queue)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一元过程，作用于队列的每个元素。
;;
;; list-queue : list-queue?
;; 源队列。
;;
;; 返回值
;; ------
;; list-queue?
;; 映射结果构成的新队列，源队列不受影响。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-map 基本测试
(let* ((q (list-queue 1 2 3)) (r (list-queue-map (lambda (x) (* x 10)) q)))
  (check (list-queue-list r) => '(10 20 30))
  ;; 原队列不受影响
  (check (list-queue-list q) => '(1 2 3))
) ;let*


;; 空队列
(let ((q (list-queue-map (lambda (x) (* x 2)) (list-queue))))
  (check (list-queue-empty? q) => #t)
) ;let


;; 单元素队列
(let ((r (list-queue-map (lambda (x) (+ x 1)) (list-queue 42))))
  (check (list-queue-list r) => '(43))
) ;let


;; 返回不同类型
(let ((r (list-queue-map (lambda (x) (if (odd? x) 'odd 'even)) (list-queue 1 2 3))))
  (check (list-queue-list r) => '(odd even odd))
) ;let


(check-report)
