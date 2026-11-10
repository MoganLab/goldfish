;; 有效匹配的小括号示例
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(let ((a 1) (b 2))
  (display (+ a b))
  (newline))

(list 1 2 3 4 5)
(list (quote a) (quote b) (quote c))