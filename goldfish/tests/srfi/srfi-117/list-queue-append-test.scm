;; list-queue-append
;; 依次拼接若干队列，返回一个全新的队列。
;;
;; 语法
;; ----
;; (list-queue-append . list-queues)
;;
;; 参数
;; ----
;; list-queues : (list-queue? ...)
;; 待拼接的队列，元素按参数顺序进入结果队列。
;;
;; 返回值
;; ------
;; list-queue?
;; 拼接结果，是一个新队列，原队列不受影响。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-append 基本测试
(let ((x (list-queue 1 2 3)) (y (list-queue 4 5)))
  (let ((z (list-queue-append x y)))
    (check (list-queue-list z) => '(1 2 3 4 5))
    ;; 原队列不受影响
    (check (list-queue-list x) => '(1 2 3))
    (check (list-queue-list y) => '(4 5))
  ) ;let
) ;let


;; 空队列
(let ((q1 (list-queue 1 2)) (q2 (list-queue)))
  (let ((z (list-queue-append q1 q2)))
    (check (list-queue-list z) => '(1 2))
  ) ;let
) ;let


;; 多个队列
(let ((q1 (list-queue 1)) (q2 (list-queue 2)) (q3 (list-queue 3)))
  (let ((z (list-queue-append q1 q2 q3)))
    (check (list-queue-list z) => '(1 2 3))
  ) ;let
) ;let


;; 单个队列
(let ((q (list-queue 1 2 3)))
  (let ((z (list-queue-append q)))
    (check (list-queue-list z) => '(1 2 3))
  ) ;let
) ;let


(check-report)
