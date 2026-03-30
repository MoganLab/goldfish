(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-size
;; 获取 set 中元素的数量。
;;
;; 语法
;; ----
;; (set-size set)
;;
;; 参数
;; ----
;; set : set
;; 要获取大小的 set。
;;
;; 返回值
;; ----
;; exact-integer
;; 返回 set 中元素的数量（整数）。
;;
;; 示例
;; ----
;; (set-size (set)) => 0
;; (set-size (set 1 2 3)) => 3
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。

(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))

(check (set-size s-empty) => 0)
(check (set-size s-1) => 1)
(check (set-size s-1-2) => 2)
(check (set-size s-1-2-3) => 3)
(check (set-size s-2-3-4) => 3)
(check (set-size s-4-5) => 2)

;; Large set test
(define (range n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc))
    ) ;if
  ) ;let
) ;define

(define big-n 1000000)
(define big-list (range big-n))
(define s-big (list->set big-list))
(define s-small-big (list->set (range (- big-n 1))))

(check (set-size s-big) => big-n)
(check (set-size s-small-big) => (- big-n 1))

(check-catch 'type-error (set-size "not a set"))

(check-report)
