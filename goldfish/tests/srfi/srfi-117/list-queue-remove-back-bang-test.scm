;; list-queue-remove-back!
;; 移除并返回队列尾部的元素。
;;
;; 语法
;; ----
;; (list-queue-remove-back! list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; any
;; 被移除的队尾元素。
;;
;; 错误处理
;; --------
;; 对空队列调用会抛出 type-error。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-remove-back! 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-back! q) => 3)
  (check (list-queue-list q) => '(1 2))
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-remove-back! q) => 42)
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除所有元素
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-back! q) => 3)
  (check (list-queue-remove-back! q) => 2)
  (check (list-queue-remove-back! q) => 1)
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除后可以继续添加
(let ((q (list-queue 1 2)))
  (list-queue-remove-back! q)
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 3))
) ;let


;; 空队列报错
(check-catch 'type-error (list-queue-remove-back! (list-queue)))


(check-report)
