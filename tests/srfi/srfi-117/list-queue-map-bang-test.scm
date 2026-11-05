;; list-queue-map!
;; 用一元过程原地改写队列中的每个元素。
;;
;; 语法
;; ----
;; (list-queue-map! proc list-queue)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一元过程，作用于队列的每个元素。
;;
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未定义，效果为队列元素被原地替换。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-map! 基本测试
(let ((q (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (* x 10)) q)
  (check (list-queue-list q) => '(10 20 30))
) ;let


;; 空队列
(let ((q (list-queue)))
  (list-queue-map! (lambda (x) (* x 2)) q)
  (check (list-queue-empty? q) => #t)
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (list-queue-map! (lambda (x) (+ x 1)) q)
  (check (list-queue-list q) => '(43))
) ;let


;; 修改后可以继续操作
(let ((q (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (* x 2)) q)
  (list-queue-add-back! q 100)
  (check (list-queue-list q) => '(2 4 6 100))
) ;let


(check-report)
