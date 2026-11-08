;; delay
;; 延迟求值：构造一个 promise，封装 expression 的求值。
;;
;; 语法
;; ----
;; (delay <expression>)
;;
;; 参数
;; ----
;; <expression> : any
;; 被延迟求值的表达式。
;;
;; 返回值
;; ----
;; promise?
;; 一个 promise 对象，其值在首次被 force 时求值并记忆化。
;;
;; 说明
;; ----
;; R7RS 4.2.5：(delay expression) 等价于
;; (make-promise (lambda () expression))，语义来自 SRFI 45。
;;
;; 1. 求值恰好发生一次，结果被记忆化：
;;   (force (delay (begin (set! n (+ n 1)) n))) 两次 force 后 n 只增加 1
;; 2. 重入 force（R5RS reentrancy test 1）：
;;   promise 的表达式内部再次 force 同一 promise 时，
;;   内层求值完成后外层采用其记忆化结果
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (scheme lazy) (scheme base))

(check-set-mode! 'report-failed)

;; 1. 基本延迟求值与记忆化（SRFI 45 memoization test 1/2）
(define n1 0)
(define p1 (delay (begin (set! n1 (+ n1 1)) n1)))
(check (force p1) => 1)
(check (force p1) => 1)
(check n1 => 1)

(let ((n2 0))
  (define p2 (delay (begin (set! n2 (+ n2 1)) n2)))
  (check (+ (force p2) (force p2)) => 2)
  (check n2 => 1)
) ;let

;; 2. 重入 force（SRFI 45 reentrancy test 1，出自 R5RS）
(define r-count 0)
(define r-x 5)
(define r-p
  (delay (begin (set! r-count (+ r-count 1))
                (if (> r-count r-x) r-count (force r-p)))))
(check (force r-p) => 6)
(set! r-x 10)
(check (force r-p) => 6)
(check r-count => 6)

;; 3. 重入 force（SRFI 45 reentrancy test 2，出自 SRFI 40）
(define f2
  (let ((first? #t))
    (delay
      (if first?
        (begin (set! first? #f) (force f2))
        'second))))
(check (force f2) => 'second)

;; 4. 重入 force（SRFI 45 reentrancy test 3，John Shutt）
(define q3-count 5)
(define q3-p #f)
(set! q3-p
  (delay (if (<= q3-count 0)
           q3-count
           (begin (set! q3-count (- q3-count 1))
                  (force q3-p)
                  (set! q3-count (+ q3-count 2))
                  q3-count))))
(check q3-count => 5)
(check (force q3-p) => 0)
(check q3-count => 10)

(check-report)
