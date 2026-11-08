;; list-queue-add-front!
;; 向队列头部插入元素，原地修改队列。
;;
;; 语法
;; ----
;; (list-queue-add-front! list-queue elem)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; elem : any
;; 待插入的元素。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未定义，效果为队列被原地修改。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-add-front! 基本测试
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-front q) => 1)
) ;let


;; 添加到空队列
(let ((q (list-queue)))
  (list-queue-add-front! q 'a)
  (check (list-queue-list q) => '(a))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'a)
) ;let


;; 多次添加
(let ((q (list-queue)))
  (list-queue-add-front! q 3)
  (list-queue-add-front! q 2)
  (list-queue-add-front! q 1)
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 不影响后端
(let ((q (list-queue 2 3)))
  (list-queue-add-front! q 1)
  (check (list-queue-back q) => 3)
) ;let


(check-report)
