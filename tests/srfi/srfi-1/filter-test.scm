;; filter
;; 过滤列表中满足谓词的元素。
;;
;; 语法
;; ----
;; (filter pred list)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; list : list?
;; 要过滤的列表。
;;
;; 返回值
;; ------
;; list
;; 包含所有满足谓词条件的元素的新列表，保持原有顺序。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (filter even? '(-2 -1 0 1 2)) => '(-2 0 2))
(check (filter even? '(1 2 3 4 5 6)) => '(2 4 6))
(check (filter odd? '(1 2 3 4 5 6)) => '(1 3 5))
(check (filter positive? '(-2 -1 0 1 2)) => '(1 2))
(check (filter negative? '(-2 -1 0 1 2)) => '(-2 -1))

(check (filter even? '()) => '())
(check (filter (lambda (x) #f) '(1 2 3)) => '())
(check (filter (lambda (x) #t) '(1 2 3)) => '(1 2 3))
(check (filter (lambda (x) (> x 100)) '(1 2 3)) => '())

(check (filter symbol? '(a 1 b 2 c 3)) => '(a b c))
(check (filter string? '("hello" 42 "world" 3.14)) => '("hello" "world"))
(check (filter list? '(1 (2 3) 4 (5 (6)))) => '((2 3) (5 (6))))
(check (filter boolean? '(#t #f 1 "a" #t)) => '(#t #f #t))

(check (filter (lambda (x) (and (list? x) (not (null? x)))) '(() (a) b (c d) ()))
  =>
  '((a) (c d)))
(check (filter (lambda (x) (and (list? x) (> (length x) 1))) '((a)
                                                               (b c)
                                                               (d)
                                                               (e f g)))
  =>
  '((b c) (e f g)))

(check (filter (lambda (x) (and x (> x 5))) '(3 7 4 9 2 8)) => '(7 9 8))
(check (filter (lambda (x) (> (string-length x) 3)) '("a" "hello" "xyz" "world"))
  =>
  '("hello" "world"))

(let ((numbers (iota 100)))
  (check (filter (lambda (x) (= (modulo x 10) 0)) numbers)
    =>
    '(0 10 20 30 40 50 60 70 80 90)))

(let ((large-list (make-list 1000 5)))
  (check (length (filter (lambda (x) #t) large-list)) => 1000)
  (check (length (filter (lambda (x) (> x 0)) large-list)) => 1000)
  (check (length (filter (lambda (x) (> x 5)) large-list)) => 0))

;; 结构共享：全部匹配时返回原列表本身
(let ((l (list 1 2 3)))
  (check (eq? (filter (lambda (x) #t) l) l) => #t))

;; 结构共享：末尾连续匹配的最长后缀与原列表共享
(let* ((l (list 1 2 3 4 5 6)) (r (filter (lambda (x) (> x 3)) l)))
  (check r => '(4 5 6))
  (check (eq? r (cdddr l)) => #t))

;; 错误用例：非列表参数
(check-catch 'type-error (filter even? 5))
(check-catch 'type-error (filter even? '(1 2 . 3)))

;; 空列表不会调用 pred，pred 不是函数也不报错
(check (filter 5 '()) => '())

;; GC 压力回归测试：闭包 pred + 大量临时 cons，反复触发 GC
;; （曾导致 g_filter 中 pred 被误回收：attempt to apply a free cell）
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (filter (lambda (x) (< x 5000)) l)
    (filter (lambda (x) (>= x 5000)) l)
    (filter (lambda (x) #f) l))
  (check (length (filter (lambda (x) (< x 5000)) l)) => 5000)
  (check (length (filter (lambda (x) (>= x 5000)) l)) => 5000)
  (check (filter (lambda (x) #f) l) => '()))

(check-report)
