;; list-sort! 性能基准测试
;; 对比旧 Scheme 快速排序实现（已修正丢弃 pivot 的 bug，以便公平对比）
;; 与新实现（基于 S7 内置 sort!，C qsort）

(import (liii timeit) (scheme base) (liii sort) (liii list))

;; 旧 Scheme 实现（srfi-132 中的快速排序，修正了丢弃 pivot 的 bug）

(define (list-sort!-scheme less-p lst)
  (define (partition! lst pivot less-p)
    (let loop
      ((lst lst) (less '()) (greater '()))
      (cond ((null? lst) (values (reverse less) (reverse greater)))
            ((less-p (car lst) pivot) (loop (cdr lst) (cons (car lst) less) greater))
            (else (loop (cdr lst) less (cons (car lst) greater)))
      ) ;cond
    ) ;let
  ) ;define
  (if (or (null? lst) (null? (cdr lst)))
    lst
    (let* ((pivot (car lst)))
      (call-with-values (lambda () (partition! (cdr lst) pivot less-p))
        (lambda (less greater)
          (let ((sorted-less (list-sort!-scheme less-p less))
                (sorted-greater (list-sort!-scheme less-p greater))
               ) ;
            (if (null? sorted-less)
              (cons pivot sorted-greater)
              (begin
                (set-cdr! (last-pair sorted-less) (cons pivot sorted-greater))
                sorted-less
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
      ) ;call-with-values
    ) ;let*
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

;; 确定性伪随机列表（线性同余生成器），保证两次实现面对相同数据

(define (make-random-list n)
  (let loop
    ((i n) (seed 12345) (acc '()))
    (if (= i 0)
      acc
      (let ((next (modulo (+ (* seed 1103515245) 12345) 2147483648)))
        (loop (- i 1) next (cons (modulo next 100000) acc))
      ) ;let
    ) ;if
  ) ;let
) ;define

(define data-random-1000 (make-random-list 1000))

(define data-random-10000 (make-random-list 10000))

(define data-sorted-1000 (iota 1000))

(define data-reverse-1000 (reverse (iota 1000)))

(define (run-benchmarks)
  (display "=== list-sort! 性能测试 ===\n\n")

  ;; 随机数据（1000 元素）
  (bench "Scheme 随机列表(1000)   "
    (lambda () (list-sort!-scheme < (append data-random-1000 '())))
    100
  ) ;bench
  (bench "sort!  随机列表(1000)   "
    (lambda () (list-sort! < (append data-random-1000 '())))
    100
  ) ;bench

  ;; 随机数据（10000 元素）
  (bench "Scheme 随机列表(10000)  "
    (lambda () (list-sort!-scheme < (append data-random-10000 '())))
    10
  ) ;bench
  (bench "sort!  随机列表(10000)  "
    (lambda () (list-sort! < (append data-random-10000 '())))
    10
  ) ;bench

  ;; 已排序数据（快速排序最坏情况之一：pivot 恒为最小值）
  (bench "Scheme 有序列表(1000)   "
    (lambda () (list-sort!-scheme < (append data-sorted-1000 '())))
    100
  ) ;bench
  (bench "sort!  有序列表(1000)   "
    (lambda () (list-sort! < (append data-sorted-1000 '())))
    100
  ) ;bench

  ;; 逆序数据
  (bench "Scheme 逆序列表(1000)   "
    (lambda () (list-sort!-scheme < (append data-reverse-1000 '())))
    100
  ) ;bench
  (bench "sort!  逆序列表(1000)   "
    (lambda () (list-sort! < (append data-reverse-1000 '())))
    100
  ) ;bench
) ;define

(run-benchmarks)
