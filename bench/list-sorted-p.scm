;; list-sorted? 性能基准测试
;; 对比旧 Scheme 实现（do 循环逐对比较）
;; 与新实现（liii_sort.cpp 的 g_list-sorted?）

(import (liii timeit) (scheme base) (liii sort))

;; 旧 Scheme 实现（优化前 srfi-132 中的定义）

(define (list-sorted?-scheme less-p lis)
  (if (null? lis)
    #t
    (do ((first lis (cdr first))
         (second (cdr lis) (cdr second))
         (res #t (not (less-p (car second) (car first))))
        ) ;
      ((or (null? second) (not res)) res)
    ) ;do
  ) ;if
) ;define

(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

(define sorted-long (iota 1000))

(define sorted-short '(1 2 3 4 5 6 7 8 9 10))

(define unsorted-head '(5 1 2 3 4 6 7 8 9 10))

(define unsorted-tail (append (iota 999) '(-1)))

(define (run-benchmarks)
  (display "=== list-sorted? 性能测试 ===\n\n")

  ;; 长列表全有序（最坏情况：遍历整个列表）
  (bench "Scheme 长列表有序(1000)  "
    (lambda () (list-sorted?-scheme < sorted-long))
    10000
  ) ;bench
  (bench "C++    长列表有序(1000)  "
    (lambda () (list-sorted? < sorted-long))
    10000
  ) ;bench

  ;; 短列表全有序
  (bench "Scheme 短列表有序(10)    "
    (lambda () (list-sorted?-scheme < sorted-short))
    100000
  ) ;bench
  (bench "C++    短列表有序(10)    "
    (lambda () (list-sorted? < sorted-short))
    100000
  ) ;bench

  ;; 头部即逆序（最快返回）
  (bench "Scheme 头部逆序          "
    (lambda () (list-sorted?-scheme < unsorted-head))
    100000
  ) ;bench
  (bench "C++    头部逆序          "
    (lambda () (list-sorted? < unsorted-head))
    100000
  ) ;bench

  ;; 尾部才逆序
  (bench "Scheme 尾部逆序(1000)    "
    (lambda () (list-sorted?-scheme < unsorted-tail))
    10000
  ) ;bench
  (bench "C++    尾部逆序(1000)    "
    (lambda () (list-sorted? < unsorted-tail))
    10000
  ) ;bench

  ;; 空列表与单元素
  (bench "Scheme 空列表            "
    (lambda () (list-sorted?-scheme < '()))
    100000
  ) ;bench
  (bench "C++    空列表            " (lambda () (list-sorted? < '())) 100000)

  ;; 自定义 lambda 比较器（回调开销主导）
  (bench "Scheme lambda比较器(100) "
    (lambda () (list-sorted?-scheme (lambda (a b) (< a b)) (iota 100)))
    10000
  ) ;bench
  (bench "C++    lambda比较器(100) "
    (lambda () (list-sorted? (lambda (a b) (< a b)) (iota 100)))
    10000
  ) ;bench
) ;define

(run-benchmarks)
