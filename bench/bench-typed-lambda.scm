(import (liii base) (liii timeit))

;; bench: typed-lambda 宏 vs 手写 unless 类型校验
;;
;; 关注两点：
;;   1. 顶层 define 中的宏：加载时展开一次，调用时只剩类型检查
;;   2. 函数体内的宏：每次调用都要重新展开宏（解释器在求值时展开）

(define N 200000)

;; 1. 顶层 typed-lambda
(define typed-add
  (typed-lambda ((x integer?)) (+ x 1))
) ;define

;; 1. 顶层手写 unless
(define manual-add
  (lambda (x)
    (unless (integer? x)
      (error 'type-error "manual-add: x must be an integer" x)
    ) ;unless
    (+ x 1)
  )
) ;define

;; 2. 函数体内的宏，每次调用都重新展开
(define (call-typed-inner x)
  (define inner (typed-lambda ((y integer?)) (+ y 1)))
  (inner x)
) ;define

;; 2. 函数体内的普通 lambda
(define (call-manual-inner x)
  (define inner
    (lambda (y)
      (unless (integer? y)
        (error 'type-error "inner: y must be an integer" y)
      ) ;unless
      (+ y 1)
    )
  ) ;define
  (inner x)
) ;define

(define (bench name proc)
  (let ((t (timeit (lambda () (proc 1)) (lambda () #t) N)))
    (display name)
    (display " : ")
    (display t)
    (display " ms")
    (newline)
  ) ;let
) ;define

(display "loop count: ")
(display N)
(newline)

(bench "top-level typed-lambda " typed-add)
(bench "top-level manual unless " manual-add)
(bench "in-body  typed-lambda " call-typed-inner)
(bench "in-body  manual unless " call-manual-inner)

;; 宏展开本身的成本
(let ((t (timeit
           (lambda () (eval '(typed-lambda ((x integer?)) (+ x 1))))
           (lambda () #t)
           10000
         )))
  (display "macro expansion only  : ")
  (display t)
  (display " s / 10000 expansions")
  (newline)
) ;let
