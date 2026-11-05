;; list-queue-back
;; 取出队列尾部的元素，但不移除它。
;;
;; 语法
;; ----
;; (list-queue-back list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; any
;; 队列最后一个元素。
;;
;; 错误处理
;; --------
;; 对空队列调用会抛出 type-error。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-back 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-back q) => 3)
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-back q) => 42)
  (check (list-queue-front q) => 42)
) ;let


;; 后端添加后，back 变化
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-back q) => 3)
) ;let


;; 后端移除后，back 变化
(let ((q (list-queue 1 2 3)))
  (list-queue-remove-back! q)
  (check (list-queue-back q) => 2)
) ;let


;; 前端添加不影响 back
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-back q) => 3)
) ;let


;; 空队列报错
(check-catch 'type-error (list-queue-back (list-queue)))


(check-report)
