;; list-queue-add-back!
;; 向队列尾部追加元素，原地修改队列。
;;
;; 语法
;; ----
;; (list-queue-add-back! list-queue elem)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; elem : any
;; 待追加的元素。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未定义，效果为队列被原地修改。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-add-back! 基本测试
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-back q) => 3)
) ;let


;; 添加到空队列
(let ((q (list-queue)))
  (list-queue-add-back! q 'a)
  (check (list-queue-list q) => '(a))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'a)
) ;let


;; 多次添加
(let ((q (list-queue)))
  (list-queue-add-back! q 1)
  (list-queue-add-back! q 2)
  (list-queue-add-back! q 3)
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 不影响前端
(let ((q (list-queue 1 2)))
  (list-queue-add-back! q 3)
  (check (list-queue-front q) => 1)
) ;let


(check-report)
