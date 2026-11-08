;; delay-force
;; 构造递归延迟求值的 promise，保证迭代惰性算法在有界空间内运行。
;;
;; 语法
;; ----
;; (delay-force <expression>)
;;
;; 参数
;; ----
;; <expression> : any
;; 被延迟求值的表达式，求值结果通常是一个 promise。
;;
;; 返回值
;; ----
;; promise?
;; 一个 promise 对象。
;;
;; 说明
;; ----
;; R7RS 4.2.5：(delay-force expression) 概念上等价于
;; (delay (force expression))，区别在于 forcing 其结果时，
;; 对 (force expression) 的求值保证处于尾位置，
;; 因此跨越 delay-force 链的迭代不会累积栈或内存（SRFI 45 语义）。
;;
;; 1. 深度 delay-force 链在有界栈空间内完成：
;;   (define (count-down n) (delay-force (if (zero? n) 'done (count-down (- n 1)))))
;;   (force (count-down 100000)) => done
;; 2. 与 SRFI 45 的 lazy 一样，跳转链条共享最终记忆化结果：
;;   对链条尾部 promise 的单独 force 不会重新求值
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (scheme lazy) (scheme base))

(check-set-mode! 'report-failed)

;; 1. 概念等价性：(delay-force e) 行为同 (delay (force e))

(define df-eager (delay-force (delay 42)))
(check (force df-eager) => 42)

;; force 原值（非 promise）
(check (force (delay-force 7)) => 7)

;; 2. 链条共享记忆化（SRFI 45 memoization test 3 的 delay-force 版本）

(define m-n 0)

(define m-r (delay (begin (set! m-n (+ m-n 100)) 'hi)))

(define m-s (delay-force m-r))

(define m-t (delay-force m-s))
(check (force m-t) => 'hi)
(check (force m-r) => 'hi)
(check m-n => 100)

;; 3. 空间安全：深度递归链在尾位置求值，不爆栈

(define (count-down n)
  (delay-force (if (zero? n) 'done (count-down (- n 1))))
) ;define
(check (force (count-down 100000)) => 'done)

;; 4. 空间安全：安全遍历无限流（SRFI 45 leak test 3）

(define (from n)
  (delay (cons n (from (+ n 1))))
) ;define

(define (traverse s)
  (delay-force
    (let ((cell (force s)))
      (if (= (car cell) 50000) 'reached (traverse (cdr cell)))
    ) ;let
  ) ;delay-force
) ;define
(check (force (traverse (from 0))) => 'reached)

;; 5. R7RS 4.2.5 官方 stream-filter 示例（节选）

(define (stream-filter p? s)
  (delay-force
    (let ((cell (force s)))
      (cond ((null? cell) '())
            ((p? (car cell)) (cons (car cell) (stream-filter p? (cdr cell))))
            (else (stream-filter p? (cdr cell)))
      ) ;cond
    ) ;let
  ) ;delay-force
) ;define

(define 0to9
  (let build
    ((i 0))
    (delay
      (if (> i 9) '() (cons i (build (+ i 1))))
    ) ;delay
  ) ;let
) ;define

(define odd-stream (stream-filter odd? 0to9))
(check (car (force odd-stream)) => 1)
(check
  (car (force (cdr (force odd-stream))))
  =>
  3
) ;check
(check
  (car
    (force
      (cdr (force (cdr (force odd-stream))))
    ) ;force
  ) ;car
  =>
  5
) ;check
;; 逐层展开到流尾

(define sf-tail1 (force odd-stream))

(define sf-tail2 (force (cdr sf-tail1)))

(define sf-tail3 (force (cdr sf-tail2)))

(define sf-tail4 (force (cdr sf-tail3)))

(define sf-tail5 (force (cdr sf-tail4)))

(define sf-tail6 (force (cdr sf-tail5)))
(check (car sf-tail2) => 3)
(check (car sf-tail3) => 5)
(check (car sf-tail4) => 7)
(check (car sf-tail5) => 9)
(check sf-tail6 => '())

(check-report)
