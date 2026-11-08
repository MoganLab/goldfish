;; list-queue-set-list!
;; 用给定列表替换队列的全部内容。
;;
;; 语法
;; ----
;; (list-queue-set-list! list-queue first . rest)
;;
;; 参数
;; ----
;; list-queue : list-queue?
;; 目标队列。
;;
;; first : list?
;; 新的元素列表。
;;
;; rest : (last)
;; 可选参数。last 为 first 的最后一个点对，用于省略一次遍历。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未定义，效果为队列的内容被替换。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-set-list! 基本测试（单参数）
(let ((q (list-queue 1 2 3)))
  (list-queue-set-list! q '(a b c))
  (check (list-queue-list q) => '(a b c))
  (check (list-queue-front q) => 'a)
  (check (list-queue-back q) => 'c)
) ;let


;; 设置为空列表
(let ((q (list-queue 1 2 3)))
  (list-queue-set-list! q '())
  (check (list-queue-empty? q) => #t)
) ;let


;; 双参数形式
(let* ((lst '(1 2 3)) (last-ptr (cddr lst)) (q (list-queue)))
  (list-queue-set-list! q lst last-ptr)
  (check (list-queue-list q) => '(1 2 3))
) ;let*


;; 设置后队列正常工作
(let ((q (list-queue)))
  (list-queue-set-list! q '(1 2))
  (list-queue-add-back! q 3)
  (list-queue-add-front! q 0)
  (check (list-queue-list q) => '(0 1 2 3))
) ;let


(check-report)
