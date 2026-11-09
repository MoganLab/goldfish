;; list-queue-front
;; 取出队列头部的元素，但不移除它。
;;
;; 语法
;; ----
;; (list-queue-front list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; any
;; 队列第一个元素。
;;
;; 错误处理
;; --------
;; 对空队列调用会抛出 type-error。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-front 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-front q) => 1)
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (check (list-queue-front q) => 42)
) ;let


;; 不同类型
(let ((q (list-queue 'a "b" 3)))
  (check (list-queue-front q) => 'a)
) ;let


;; 前端添加后，front 变化
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-front q) => 1)
) ;let


;; 前端移除后，front 变化
(let ((q (list-queue 1 2 3)))
  (list-queue-remove-front! q)
  (check (list-queue-front q) => 2)
) ;let


;; 空队列报错
(check-catch 'type-error (list-queue-front (list-queue)))


(check-report)
