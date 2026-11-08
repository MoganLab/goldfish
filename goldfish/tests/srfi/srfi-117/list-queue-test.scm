;; list-queue
;; 由任意多个对象构造双端队列。
;;
;; 语法
;; ----
;; (list-queue . objs)
;;
;; 参数
;; ----
;; objs : any
;; 作为队列元素的任意对象，顺序即为队列中的顺序。
;;
;; 返回值
;; ------
;; list-queue?
;; 由 objs 按给定顺序构成的新队列。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 空队列
(let ((q (list-queue)))
  (check (list-queue-empty? q) => #t)
) ;let


;; 单个元素
(let ((q (list-queue 42)))
  (check (list-queue-front q) => 42)
  (check (list-queue-back q) => 42)
) ;let


;; 多个不同类型的元素
(let ((q (list-queue 'a "hello" 123 #t)))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => #t)
) ;let


(check-report)
