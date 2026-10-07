(import (liii base)
        (liii cut)
        (liii timeit)
        (srfi srfi-1))

;; ========================================================
;; 基准测试: receive / cut / cute 性能对比 (define-syntax vs define-macro)
;; ========================================================

;; --- 旧版 define-macro 实现 ---

(define-macro (old-receive formals expression . body)
  `(call-with-values (lambda ,() (values ,expression))
     (lambda ,formals ,@body))
)

(define-macro (old-cut . paras)
  (letrec* ((slot? (lambda (x) (equal? '<> x)))
            (more-slot? (lambda (x) (equal? '<...> x)))
            (slots (filter slot? paras))
            (more-slots (filter more-slot? paras))
            (xs (map (lambda (x) (gensym)) slots))
            (rest (gensym))
            (parse
              (lambda (xs paras)
                (cond ((null? paras) paras)
                      ((not (list? paras)) paras)
                      ((more-slot? (car paras)) `(,rest
                                                  ,@(parse xs (cdr paras))))
                      ((slot? (car paras)) `(,(car xs)
                                             ,@(parse (cdr xs) (cdr paras))))
                      (else
                        `(,(car paras) ,@(parse xs (cdr paras)))
                      ) ;else
                ) ;cond
              ) ;lambda
            ) ;parse
           ) ;
    (cond
     ((null? more-slots) `(lambda ,xs ,(parse xs paras)))
     (else
       (when
         (or (> (length more-slots) 1) (not (more-slot? (last paras))))
         (error 'syntax-error "<...> must be the last parameter of cut")
       ) ;when
       (let ((parsed (parse xs paras)))
         `(lambda (,@xs . ,rest) (apply ,@parsed))
       ) ;let
     ) ;else
    ) ;cond
  ) ;letrec*
)

(define-macro (old-cute . paras)
  (letrec* ((slot? (lambda (x) (equal? '<> x)))
            (more-slot? (lambda (x) (equal? '<...> x)))
            (exprs
              (filter
                (lambda (x) (not (or (slot? x) (more-slot? x))))
                paras
              ) ;filter
            ) ;exprs
            (xs (map (lambda (x) (gensym)) exprs))
            (lets (map list xs exprs))
            (parse
              (lambda (xs paras)
                (cond ((null? paras) paras)
                      ((not (list? paras)) paras)
                      ((not (or (slot? (car paras)) (more-slot? (car paras))))
                       `(,(car xs) ,@(parse (cdr xs) (cdr paras)))
                      ) ;
                      (else
                        `(,(car paras) ,@(parse xs (cdr paras)))
                      ) ;else
                ) ;cond
              ) ;lambda
            ) ;parse
           ) ;
    `(let ,lets (old-cut ,@(parse xs paras)))
  ) ;letrec*
)

;; --- 单步过程式展开 cut 原型 (用于对比消除递归层级后的性能) ---

(define-syntax single-pass-cut
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((slots-or-exprs (cdr expr))
             (slot? (lambda (x) (and (identifier? x) (compare x '<>))))
             (rest? (lambda (x) (and (identifier? x) (compare x '<...>))))
             (_lambda (rename 'lambda))
             (_apply (rename 'apply))
            )
        (let loop ((items slots-or-exprs)
                   (params '())
                   (args '()))
          (cond
            ((null? items)
             (list _lambda (reverse params) (reverse args)))
            ((and (pair? items) (rest? (car items)) (null? (cdr items)))
             (let ((rest-var (rename 'rest)))
               (list _lambda
                     (append (reverse params) rest-var)
                     (list* _apply (car (reverse args)) (append (cdr (reverse args)) (list rest-var))))))
            ((and (pair? items) (slot? (car items)))
             (let ((x (rename (gensym "x"))))
               (loop (cdr items) (cons x params) (cons x args))))
            (else
             (loop (cdr items) params (cons (car items) args)))))))))

(define (run-bench name new-thunk old-thunk count)
  (let ((t-new (timeit new-thunk (lambda () #t) count))
        (t-old (timeit old-thunk (lambda () #t) count)))
    (display name)
    (newline)
    (display "  new: ")
    (display t-new)
    (display " s")
    (newline)
    (display "  old: ")
    (display t-old)
    (display " s")
    (newline)
    (display "  speedup (old / new): ")
    (display (/ t-old t-new))
    (if (< (/ t-old t-new) 0.8)
        (display "  [SLOWER]")
        (if (> (/ t-old t-new) 1.2)
            (display "  [FASTER]")
            (display "  [COMPARABLE]")))
    (newline)
    (newline)))

(display "============================================================")
(newline)
(display "Benchmark: receive / cut / cute (define-syntax vs define-macro)")
(newline)
(display "============================================================")
(newline)
(newline)

;; -------------------- 1. receive 基准测试 --------------------

(define (bench-receive-new count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (receive (a b) (values i 1)
        (set! sum (+ sum a b))))))

(define (bench-receive-old count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (old-receive (a b) (values i 1)
        (set! sum (+ sum a b))))))

(run-bench "[receive] 循环内展开+执行 (10,000 次)"
           (lambda () (bench-receive-new 10000))
           (lambda () (bench-receive-old 10000))
           5)

(run-bench "[receive] eval 动态展开求值 (1,000 次)"
           (lambda () (eval '(receive (a b) (values 1 2) (+ a b))))
           (lambda () (eval '(old-receive (a b) (values 1 2) (+ a b))))
           1000)

;; -------------------- 2. cut 基准测试 --------------------

(define (bench-cut-create-and-call-new count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (let ((f (cut + 1 <> 2 <>)))
        (set! sum (+ sum (f i 3)))))))

(define (bench-cut-create-and-call-old count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (let ((f (old-cut + 1 <> 2 <>)))
        (set! sum (+ sum (f i 3)))))))

(define (bench-cut-create-and-call-sp count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (let ((f (single-pass-cut + 1 <> 2 <>)))
        (set! sum (+ sum (f i 3)))))))

(run-bench "[cut] 循环内创建闭包并调用 (10,000 次)"
           (lambda () (bench-cut-create-and-call-new 10000))
           (lambda () (bench-cut-create-and-call-old 10000))
           5)

(run-bench "[cut] 优化单步展开 vs 旧版宏: 循环创建+调用 (10,000 次)"
           (lambda () (bench-cut-create-and-call-sp 10000))
           (lambda () (bench-cut-create-and-call-old 10000))
           5)

(define fn-cut-new (cut + 1 <> 2 <>))
(define fn-cut-old (old-cut + 1 <> 2 <>))

(define (bench-cut-call-only fn count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (set! sum (+ sum (fn i 3))))))

(run-bench "[cut] 预创建闭包纯调用 (100,000 次)"
           (lambda () (bench-cut-call-only fn-cut-new 100000))
           (lambda () (bench-cut-call-only fn-cut-old 100000))
           5)

(run-bench "[cut] eval 动态展开求值 (1,000 次)"
           (lambda () (eval '((cut + 1 <> 2 <>) 3 4)))
           (lambda () (eval '((old-cut + 1 <> 2 <>) 3 4)))
           1000)

;; -------------------- 3. cute 基准测试 --------------------

(define (bench-cute-create-and-call-new count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (let ((f (cute + 1 <> 2 <>)))
        (set! sum (+ sum (f i 3)))))))

(define (bench-cute-create-and-call-old count)
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
        ((= i count) sum)
      (let ((f (old-cute + 1 <> 2 <>)))
        (set! sum (+ sum (f i 3)))))))

(run-bench "[cute] 循环内创建闭包并调用 (10,000 次)"
           (lambda () (bench-cute-create-and-call-new 10000))
           (lambda () (bench-cute-create-and-call-old 10000))
           5)

(run-bench "[cute] eval 动态展开求值 (1,000 次)"
           (lambda () (eval '((cute + 1 <> 2 <>) 3 4)))
           (lambda () (eval '((old-cute + 1 <> 2 <>) 3 4)))
           1000)
