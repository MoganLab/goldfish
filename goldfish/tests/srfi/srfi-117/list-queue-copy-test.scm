;; list-queue-copy
;; 复制一个队列，副本与原队列互不影响。
;;
;; 语法
;; ----
;; (list-queue-copy list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 待复制的队列。
;;
;; 返回值
;; ------
;; list-queue?
;; 元素相同的新队列；对副本的两端增删不会影响原队列。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-copy 基本测试
(let* ((q1 (list-queue 1 2 3)) (q2 (list-queue-copy q1)))
  (check (list-queue-list q2) => '(1 2 3))
  ;; 修改 q2 不影响 q1
  (list-queue-add-front! q2 0)
  (check (list-queue-list q1) => '(1 2 3))
  (check (list-queue-list q2) => '(0 1 2 3))
) ;let*


;; 复制空队列
(let ((q (list-queue-copy (list-queue))))
  (check (list-queue-empty? q) => #t)
) ;let


;; 复制后添加元素到原队列不影响副本
(let* ((q1 (list-queue 'a 'b)) (q2 (list-queue-copy q1)))
  (list-queue-add-back! q1 'c)
  (check (list-queue-list q1) => '(a b c))
  (check (list-queue-list q2) => '(a b))
) ;let*


(check-report)
