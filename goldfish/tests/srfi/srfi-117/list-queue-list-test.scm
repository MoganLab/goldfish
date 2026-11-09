;; list-queue-list
;; 以列表形式返回队列中的全部元素。
;;
;; 语法
;; ----
;; (list-queue-list list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; list?
;; 与队列元素顺序一致的列表。
;;
;; 注意
;; ----
;; 返回的列表与队列内部结构共享，向队尾追加元素可能反映到已取出的列表上。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-list 基本测试
(let ((q (list-queue 1 2 3)))
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 空队列
(let ((q (list-queue)))
  (check (list-queue-list q) => '())
) ;let


;; 返回的列表与队列共享结构（修改队列会影响返回的列表）
(let ((q (list-queue 1 2 3)))
  (let ((lst (list-queue-list q)))
    (list-queue-add-back! q 4)
    ;; 添加后列表应该包含新元素
    (check lst => '(1 2 3 4))
  ) ;let
) ;let


;; 单元素
(let ((q (list-queue 42)))
  (check (list-queue-list q) => '(42))
) ;let


(check-report)
