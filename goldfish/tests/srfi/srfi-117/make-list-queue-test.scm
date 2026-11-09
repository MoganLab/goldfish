;; make-list-queue
;; 由列表构造双端队列，可选地指定表示尾部的点对。
;;
;; 语法
;; ----
;; (make-list-queue list-arg . rest)
;;
;; 参数
;; ----
;; list-arg : list?
;; 用于初始化队列的列表。
;;
;; rest : (list-queue-last)
;; 可选参数。list-queue-last 为 list-arg 的最后一个点对（last-pair），
;; 用于省略一次遍历；省略时由本函数自行求得。
;;
;; 返回值
;; ------
;; list-queue?
;; 元素顺序与 list-arg 一致的新队列。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; make-list-queue 基本测试
(let ((q (make-list-queue '(1 2 3))))
  (check (list-queue-list q) => '(1 2 3))
) ;let


;; 空列表
(let ((q (make-list-queue '())))
  (check (list-queue-empty? q) => #t)
) ;let


;; 带 last-pair 参数的 make-list-queue
(let* ((lst '(1 2 3)) (last-ptr (cddr lst)) (q (make-list-queue lst last-ptr)))
  (check (list-queue-list q) => '(1 2 3))
  (check (list-queue-back q) => 3)
) ;let*


(check-report)
