(import (scheme time) (srfi srfi-1))

(define (timing proc)
  (let ((start (current-jiffy)))
    (proc)
    (let ((end (current-jiffy)))
      (- end start)
    ) ;let
  ) ;let
) ;define

(define (repeat n proc)
  (do ((i 0 (+ i 1)))
    ((= i n))
    (proc)
  ) ;do
) ;define

;; ; 原始的 O(n²) 实现（用于对比）
(define (%extract-maybe-equal-old maybe-equal)
  (let ((my-equal (if (null-list? maybe-equal)
                    equal?
                    (car maybe-equal)
                  ) ;if
        ) ;my-equal
       ) ;
    (if (procedure? my-equal)
      my-equal
      (error 'wrong-type-arg
        "maybe-equal must be procedure"
      ) ;error
    ) ;if
  ) ;let
) ;define

(define (delete-old x l . maybe-equal)
  (let ((my-equal (%extract-maybe-equal-old maybe-equal)
        ) ;my-equal
       ) ;
    (filter (lambda (y) (not (my-equal x y)))
      l
    ) ;filter
  ) ;let
) ;define

;; ; right-duplicate deletion
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; delete-duplicates delete-duplicates!
;; ;
;; ; Beware -- these are N^2 algorithms. To efficiently remove duplicates
;; ; in long lists, sort the list to bring duplicates together, then use a
;; ; linear-time algorithm to kill the dups. Or use an algorithm based on
;; ; element-marking. The former gives you O(n lg n), the latter is linear.

(define (delete-duplicates-old
          lis
          .
          maybe-equal
        ) ;
  (let ((my-equal (%extract-maybe-equal-old maybe-equal)
        ) ;my-equal
       ) ;
    (let recur
      ((lis lis))
      (if (null-list? lis)
        lis
        (let* ((x (car lis))
               (tail (cdr lis))
               (new-tail (recur (delete-old x tail my-equal))
               ) ;new-tail
              ) ;
          (if (eq? tail new-tail)
            lis
            (cons x new-tail)
          ) ;if
        ) ;let*
      ) ;if
    ) ;let
  ) ;let
) ;define

;; ; 测试数据生成
(define (make-test-data size type)
  (case type
   ((numbers)
    (map (lambda (i)
           (modulo i (quotient size 4))
         ) ;lambda
      (iota size)
    ) ;map
   ) ;
   ((strings)
    (map (lambda (i)
           (string-append "str"
             (number->string (modulo i (quotient size 4))
             ) ;number->string
           ) ;string-append
         ) ;lambda
      (iota size)
    ) ;map
   ) ;
   ((symbols)
    (map (lambda (i)
           (string->symbol (string-append "sym"
                             (number->string (modulo i (quotient size 4))
                             ) ;number->string
                           ) ;string-append
           ) ;string->symbol
         ) ;lambda
      (iota size)
    ) ;map
   ) ;
   (else (iota size))
  ) ;case
) ;define

;; ; 性能测试函数
(define (benchmark-delete-duplicates size
          data-type
          eq-func
          repeat-count
        ) ;benchmark-delete-duplicates
  (let ((test-data (make-test-data size data-type)
        ) ;test-data
       ) ;
    (display "测试: ")
    (display size)
    (display " 元素, ")
    (display data-type)
    (display " 类型, ")
    (display eq-func)
    (display " 函数\n")

    ;; 测试新版本（优化后）
    (let ((time-new (timing (lambda ()
                              (repeat repeat-count
                                (lambda ()
                                  (delete-duplicates test-data eq-func)
                                ) ;lambda
                              ) ;repeat
                            ) ;lambda
                    ) ;timing
          ) ;time-new
         ) ;
      (display "  优化版本: ")
      (display time-new)
      (display " jiffies\n")

      ;; 测试旧版本
      (let ((time-old (timing (lambda ()
                                (repeat repeat-count
                                  (lambda ()
                                    (delete-duplicates-old test-data
                                      eq-func
                                    ) ;delete-duplicates-old
                                  ) ;lambda
                                ) ;repeat
                              ) ;lambda
                      ) ;timing
            ) ;time-old
           ) ;
        (display "  原始版本: ")
        (display time-old)
        (display " jiffies\n")
        (display "  性能提升: ")
        (display (exact->inexact (/ time-old (max time-new 1))
                 ) ;exact->inexact
        ) ;display
        (display "x\n")
      ) ;let
    ) ;let

    (newline)
  ) ;let
) ;define

;; ; 主测试
(display "=== delete-duplicates 性能测试 ===\n\n"
) ;display

;; 小数据集测试（可以对比新旧版本）
(display "--- 小数据集测试 (可对比新旧版本) ---\n"
) ;display
(benchmark-delete-duplicates 100
  'numbers
  equal?
  100
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 100
  'strings
  string=?
  100
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 100
  'symbols
  eq?
  100
) ;benchmark-delete-duplicates

;; 中等数据集测试
(display "--- 中等数据集测试 ---\n"
) ;display
(benchmark-delete-duplicates 1000
  'numbers
  equal?
  10
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 1000
  'strings
  string=?
  10
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 1000
  'symbols
  eq?
  10
) ;benchmark-delete-duplicates

;; 大数据集测试
(display "--- 大数据集测试 ---\n")
(benchmark-delete-duplicates 5000
  'numbers
  equal?
  3
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 5000
  'strings
  string=?
  3
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 5000
  'symbols
  eq?
  3
) ;benchmark-delete-duplicates

;; 超大数据集测试
(display "--- 超大数据集测试 ---\n"
) ;display
(benchmark-delete-duplicates 10000
  'numbers
  equal?
  1
) ;benchmark-delete-duplicates
(benchmark-delete-duplicates 10000
  'strings
  string=?
  1
) ;benchmark-delete-duplicates

;; 测试不同相等性函数的性能
(display "--- 不同相等性函数性能对比 ---\n"
) ;display
(let ((test-data (make-test-data 2000 'numbers)
      ) ;test-data
     ) ;
  (display "2000 数字元素:\n")
  (let ((time-eq (timing (lambda ()
                           (repeat 5
                             (lambda ()
                               (delete-duplicates test-data eq?)
                             ) ;lambda
                           ) ;repeat
                         ) ;lambda
                 ) ;timing
        ) ;time-eq
       ) ;
    (display "  eq?: ")
    (display time-eq)
    (display " jiffies\n")
  ) ;let
  (let ((time-eqv (timing (lambda ()
                            (repeat 5
                              (lambda ()
                                (delete-duplicates test-data eqv?)
                              ) ;lambda
                            ) ;repeat
                          ) ;lambda
                  ) ;timing
        ) ;time-eqv
       ) ;
    (display "  eqv?: ")
    (display time-eqv)
    (display " jiffies\n")
  ) ;let
  (let ((time-equal (timing (lambda ()
                              (repeat 5
                                (lambda ()
                                  (delete-duplicates test-data equal?)
                                ) ;lambda
                              ) ;repeat
                            ) ;lambda
                    ) ;timing
        ) ;time-equal
       ) ;
    (display "  equal?: ")
    (display time-equal)
    (display " jiffies\n")
  ) ;let
  (let ((time-= (timing (lambda ()
                          (repeat 5
                            (lambda ()
                              (delete-duplicates test-data =)
                            ) ;lambda
                          ) ;repeat
                        ) ;lambda
                ) ;timing
        ) ;time-=
       ) ;
    (display "  =: ")
    (display time-=)
    (display " jiffies\n")
  ) ;let
) ;let

(display "\n测试完成！\n")
