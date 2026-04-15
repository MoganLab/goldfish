;;; FOLD-RIGHT 函数性能基准测试
;;; Copyright (C) 2026 The Goldfish Scheme Authors
;;; 比较优化前后的性能差异

(import (scheme time)
        (liii base)
        (srfi srfi-1)
        (liii error))

;;; 旧版本 - 原始实现
(define (fold-right-old f initial . lists)
  (unless (procedure? f)
    (error 'type-error "expected procedure, got ~S" f))
  (if (or (null? lists) (any null? lists))
      initial
      (apply f 
            (append (map car lists)
                    (list (apply fold-right-old f initial (map cdr lists)))))))

;;; 新版本 - 优化实现
(define (fold-right-new f initial . lists)
  (unless (procedure? f)
    (error 'type-error "expected procedure, got ~S" f))
  
  (cond
    ((null? lists) initial)
    ((and (pair? lists) (null? (cdr lists)) (list? (car lists)))
     (let loop ((lst (car lists)))
       (if (null? lst)
           initial
           (f (car lst) (loop (cdr lst))))))
    (else
     (let loop ((lsts lists))
       (if (any null? lsts)
           initial
           (let* ((cars (map car lsts))
                  (cdrs (map cdr lsts)))
             (apply f (append cars (list (loop cdrs))))))))))

;;; 测试框架
(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display msg)
    (display (number->string (- end start)))
    (display " jiffies\n")))

(define (repeat n proc)
  (when (> n 0)
        (proc)
        (repeat (- n 1) proc)))

;;; 生成测试数据
(define (make-test-list n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; 验证正确性
(define (verify)
  (display "验证正确性...\n")
  (let ((test-list-1 (make-test-list 10))
        (test-list-2 (make-test-list 8)))
    
    ;; 测试单列表 fold-right
    (let ((result-old (fold-right-old cons '() test-list-1))
          (result-new (fold-right-new cons '() test-list-1)))
      (if (equal? result-old result-new)
          (display "✓ 单列表 fold-right(cons, '(), list) - 一致\n")
          (display "✗ 单列表 fold-right 不一致!\n")))
    
    ;; 测试单列表 fold-right 加法
    (let ((result-old (fold-right-old + 0 test-list-1))
          (result-new (fold-right-new + 0 test-list-1)))
      (if (equal? result-old result-new)
          (display "✓ 单列表 fold-right(+, 0, list) - 一致\n")
          (display "✗ 单列表 fold-right 加法不一致!\n")))
    
    ;; 测试双列表 fold-right
    (let ((result-old (fold-right-old + 0 test-list-1 test-list-2))
          (result-new (fold-right-new + 0 test-list-1 test-list-2)))
      (if (equal? result-old result-new)
          (display "✓ 双列表 fold-right(+, 0, list1, list2) - 一致\n")
          (display "✗ 双列表 fold-right 不一致!\n")))
    
    ;; 测试空列表
    (let ((result-old (fold-right-old + 42))
          (result-new (fold-right-new + 42)))
      (if (equal? result-old result-new)
          (display "✓ 空列表 fold-right(+, 42) - 一致\n")
          (display "✗ 空列表 fold-right 不一致!\n")))))

;;; 性能测试
(define (run-benchmarks)
  (display "\n=== FOLD-RIGHT 性能测试 ===\n\n")
  
  ;; 单列表小规模测试 (构造列表, 1000元素, 1000次)
  (display "单列表小规模 (构造列表, 1000元素, 1000次):\n")
  (let ((test-list (make-test-list 1000)))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 1000 (lambda () (fold-right-old cons '() test-list)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 1000 (lambda () (fold-right-new cons '() test-list))))))
  
  (display "\n")
  
  ;; 单列表中等规模测试 (加法, 5000元素, 200次)
  (display "单列表中等规模 (加法, 5000元素, 200次):\n")
  (let ((test-list (make-test-list 5000)))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 200 (lambda () (fold-right-old + 0 test-list)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 200 (lambda () (fold-right-new + 0 test-list))))))
  
  (display "\n")
  
  ;; 单列表大规模测试 (构造列表, 10000元素, 100次)
  (display "单列表大规模 (构造列表, 10000元素, 100次):\n")
  (let ((test-list (make-test-list 10000)))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 100 (lambda () (fold-right-old cons '() test-list)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 100 (lambda () (fold-right-new cons '() test-list))))))
  
  (display "\n")
  
  ;; 双列表测试 (加法, 1000+800元素, 500次)
  (display "双列表测试 (加法, 1000+800元素, 500次):\n")
  (let ((test-list-1 (make-test-list 1000))
        (test-list-2 (make-test-list 800)))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 500 (lambda () (fold-right-old + 0 test-list-1 test-list-2)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 500 (lambda () (fold-right-new + 0 test-list-1 test-list-2))))))
  
  (display "\n")
  
  ;; 三列表测试 (复杂函数, 500+400+300元素, 200次)
  (display "三列表测试 (复杂函数, 500+400+300元素, 200次):\n")
  (let ((test-list-1 (make-test-list 500))
        (test-list-2 (make-test-list 400))
        (test-list-3 (make-test-list 300))
        (complex-fn (lambda (a b c acc) (+ a b c acc))))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 200 (lambda () (fold-right-old complex-fn 0 test-list-1 test-list-2 test-list-3)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 200 (lambda () (fold-right-new complex-fn 0 test-list-1 test-list-2 test-list-3))))))
  
  (display "\n")
  
  ;; 空列表测试 (边界情况, 10000次)
  (display "空列表测试 (边界情况, 10000次):\n")
  (timing "旧版本:\t\t" 
    (lambda () (repeat 10000 (lambda () (fold-right-old + 42)))))
  (timing "新版本:\t\t" 
    (lambda () (repeat 10000 (lambda () (fold-right-new + 42)))))
  
  (display "\n")
  
  ;; 小列表测试 (短路优化, 10元素, 5000次)
  (display "小列表测试 (短路优化, 10元素, 5000次):\n")
  (let ((small-list (make-test-list 10)))
    (timing "旧版本:\t\t" 
      (lambda () (repeat 5000 (lambda () (fold-right-old cons '() small-list)))))
    (timing "新版本:\t\t" 
      (lambda () (repeat 5000 (lambda () (fold-right-new cons '() small-list)))))))

;;; 运行测试
(verify)
(run-benchmarks)
