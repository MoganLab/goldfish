(import (liii list) (liii timeit) (srfi srfi-1))

;; bench: list-take-right / list-drop-right 与旧模式 (length + srfi-1) 的对比
;;
;; 结论 (2026-08-18, len=10000)：
;;   - list-take-right 单遍快慢指针实现约比旧模式快 1.7x，已采用
;;   - list-drop-right 的 srfi-1 drop-right 是 C 实现 (g_drop_right)，
;;     Scheme 单遍实现反而慢 0.7x，故保留 length + drop-right 的写法

(define (old-take-right lst n)
  (cond ((< n 0) '())
        ((>= n (length lst)) lst)
        (else (take-right lst n))
  ) ;cond
) ;define

(define (bench name new-proc old-proc lst n number)
  (let ((t-new (timeit (lambda () (new-proc lst n)) (lambda () #t) number))
        (t-old (timeit (lambda () (old-proc lst n)) (lambda () #t) number))
       ) ;
    (display name)
    (display " : new=")
    (display t-new)
    (display " old=")
    (display t-old)
    (display " speedup=")
    (display (/ t-old t-new))
    (newline)
  ) ;let
) ;define

(define lst (iota 10000))

;; 正确性抽检
(if (not (equal? (list-take-right lst 100) (old-take-right lst 100)))
  (begin
    (display "FAILED: results differ")
    (exit 1)
  ) ;begin
) ;if

(bench "take-right  n=100, len=10000" list-take-right old-take-right lst 100
  2000
) ;bench
(bench "take-right  n=1,   len=10000" list-take-right old-take-right lst 1 2000)
