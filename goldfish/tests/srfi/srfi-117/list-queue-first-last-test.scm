;; list-queue-first-last
;; 一次返回队列内部的首点对与尾点对。
;;
;; 语法
;; ----
;; (list-queue-first-last list-queue)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; 返回值
;; ------
;; values first last
;; 返回两个值：first 为队列的表头（空队列为 '()），last 为最后一个点对
;; （空队列为 '()）。
;;
;; 注意
;; ----
;; 返回的是队列内部结构而非拷贝，仅供高效读取，不应直接改写。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-first-last 基本测试
(let ((q (list-queue 1 2 3)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '(1 2 3))
    (check last => '(3))
  ) ;let-values
) ;let


;; 空队列
(let ((q (list-queue)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '())
    (check last => '())
  ) ;let-values
) ;let


;; 单元素队列
(let ((q (list-queue 42)))
  (let-values (((first last) (list-queue-first-last q)))
    (check first => '(42))
    (check last => '(42))
    (check (eq? first last) => #t)
  ) ;let-values
) ;let


;; 验证 first 和 last 的关系
(let* ((lst '(1 2 3)) (last-ptr (cddr lst)) (q (make-list-queue lst last-ptr)))
  (let-values (((first last) (list-queue-first-last q)))
    (check (eq? first lst) => #t)
    (check (eq? last last-ptr) => #t)
  ) ;let-values
) ;let*


(check-report)
