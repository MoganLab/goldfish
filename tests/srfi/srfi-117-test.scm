(import (liii check) (srfi srfi-117))

(check-set-mode! 'report-failed)

;; 基础构造与判断

(define q1 (make-list-queue '(1 2 3)))
(check-true (list-queue? q1))
(check-false (list-queue-empty? q1))
(check (list-queue-list q1) => '(1 2 3))
(check (list-queue-front q1) => 1)
(check (list-queue-back q1) => 3)

;; 空队列测试与类型错误捕获

(define q-empty (make-list-queue '()))
(check-true (list-queue? q-empty))
(check-true (list-queue-empty? q-empty))
(check (list-queue-list q-empty) => '())
(check-catch 'type-error (list-queue-front q-empty))
(check-catch 'type-error (list-queue-back q-empty))
(check-catch 'type-error (list-queue-remove-front! q-empty))
(check-catch 'type-error (list-queue-remove-back! q-empty))

;; 添加与删除元素

(define q2 (make-list-queue '()))
(list-queue-add-front! q2 10)
(check (list-queue-front q2) => 10)
(check (list-queue-back q2) => 10)
(list-queue-add-back! q2 20)
(check (list-queue-front q2) => 10)
(check (list-queue-back q2) => 20)
(check (list-queue-remove-front! q2) => 10)
(check (list-queue-front q2) => 20)
(check (list-queue-remove-back! q2) => 20)
(check-true (list-queue-empty? q2))

;; list-queue 快捷构造与复制

(define q3 (list-queue 'a 'b 'c))
(check (list-queue-list q3) => '(a b c))

(define q3-copy (list-queue-copy q3))
(check (list-queue-list q3-copy) => '(a b c))

;; list-queue-remove-all!
(check (list-queue-remove-all! q3) => '(a b c))
(check-true (list-queue-empty? q3))

(check-report)
