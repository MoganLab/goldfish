;; drop
;; 从列表开头删除指定数量的元素。
;;
;; 语法
;; ----
;; (drop list k)
;;
;; 参数
;; ----
;; list : list?
;; 源列表，从中删除元素。
;;
;; k : integer?
;; 要从开头删除的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 删除k个元素后的新列表。当k等于列表长度时，返回空列表。
;;
;; 错误处理
;; --------
;; - out-of-range：当 k 超过列表长度或 k 为负数时抛出。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (drop '(1 2 3 4) 2) => '(3 4))
(check (drop '(1 2 3 4) 4) => '())
(check (drop '(1 2 3 . 4) 3) => 4)

(check (drop '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (drop '(1 2 3 4 5) 1) => '(2 3 4 5))
(check (drop '(1 2 3 4 5) 3) => '(4 5))
(check (drop '(1 2 3 4 5) 5) => '())

(check (drop '() 0) => '())

(check (drop '(a) 0) => '(a))
(check (drop '(a) 1) => '())

(check (drop '((a b) (c d) (e f)) 1) => '((c d) (e f)))
(check (drop '((a b) (c d) (e f)) 2) => '((e f)))
(check (drop '((a b) (c d) (e f)) 3) => '())

(check (drop '(1 2 . 3) 0) => '(1 2 . 3))
(check (drop '(1 2 . 3) 1) => '(2 . 3))
(check (drop '(1 2 . 3) 2) => 3)
(check (drop '(a b c . d) 1) => '(b c . d))
(check (drop '(a b c . d) 2) => '(c . d))
(check (drop '(a b c . d) 3) => 'd)

(check (drop (drop '(1 2 3 4 5) 1) 2) => '(4 5))
(check (drop (take '(1 2 3 4 5) 4) 2) => '(3 4))
(check (take (drop '(1 2 3 4 5) 2) 2) => '(3 4))

(let ((lst (iota 10)))
  (check (drop lst 0) => '(0 1 2 3 4 5 6 7 8 9))
  (check (drop lst 5) => '(5 6 7 8 9))
  (check (drop lst 10) => '()))

(let ((lst '(1 2 3 4 5 6 7 8 9 10)))
  (define (symmetry-test lst k)
    (let ((take-part (take lst k))
          (drop-part (drop lst k)))
      (append take-part drop-part)))

  (check (symmetry-test lst 0) => lst)
  (check (symmetry-test lst 3) => lst)
  (check (symmetry-test lst 10) => lst)
  (check (symmetry-test lst 5) => lst))

(check-catch 'out-of-range (drop '(1 2 3 4) 5))
(check-catch 'out-of-range (drop '(1 2 3 . 4) 4))
(check-catch 'out-of-range (drop '(1 2 3 4) -1))
(check-catch 'type-error (drop "not a list" 2))
(check-catch 'type-error (drop '(1 2 3) "not a number"))

(check-report)
