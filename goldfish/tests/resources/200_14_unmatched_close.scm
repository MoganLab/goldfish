;; 未匹配的右括号
(define (factorial n)
  (if (<= n 1)
      1))
      (* n (factorial (- n 1))))

(define (square x) (* x x)))

(let ((a 1) (b 2)))
  (display (+ a b))
  (newline))