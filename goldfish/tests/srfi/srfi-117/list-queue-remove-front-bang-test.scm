;; list-queue-remove-front!
;; 移除并返回队列头部的元素。
;;
;; 语法
;; ----
;; (list-queue-remove-front! list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; any
;; 被移除的队首元素。
;;
;; 错误处理
;; --------
;; 对空队列调用会抛出 type-error。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-remove-front! 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-front! q) => 1)
  (check (list-queue-list q) => '(2 3))
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-remove-front! q) => 42)
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除所有元素
(let ((q (list-queue 1 2 3)))
  (check (list-queue-remove-front! q) => 1)
  (check (list-queue-remove-front! q) => 2)
  (check (list-queue-remove-front! q) => 3)
  (check (list-queue-empty? q) => #t)
) ;let


;; 移除后可以继续添加
(let ((q (list-queue 1 2)))
  (list-queue-remove-front! q)
  (list-queue-add-front! q 0)
  (check (list-queue-list q) => '(0 2))
) ;let


;; 空队列报错
(check-catch 'type-error (list-queue-remove-front! (list-queue)))


(check-report)
