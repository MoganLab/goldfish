;; list%for-each
;; 遍历执行副作用（对标 Scala List.foreach）。
;;
;; 语法
;; ----
;; ($ xs :for-each f)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; f : procedure?
;; 单参函数，为每个元素执行一次；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; list?
;; 返回原链表 xs 本身（保持可链），副作用经由闭包观察。
;;
;; 描述
;; ----
;; 去糖为 (for-each f xs) 后透传 xs。
;; 与 Scala foreach 返回 Unit 不同，此处返回原集合以支持链式书写，
;; 属本库的刻意设计偏离。

(import (liii check) (liii lang) (scheme base))

(check-set-mode! 'report-failed)

;; :for-each 返回原集合（可链），副作用经由闭包观察
(let ((acc '()))
  (check
    ($ '(1 2 3) :for-each (lambda (x) (set! acc (cons x acc))))
    =>
    '(1 2 3)
  ) ;check
  (check acc => '(3 2 1))
) ;let

(check ($ '() :for-each (lambda (x) x)) => '())
(check ($ '(1 2 3) :for-each (lambda (x) (* x 2)) :sum) => 6)

(check-report)
