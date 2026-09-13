;; agree-01: core basics. Single datum; final form must be (display ...).
;; Runs under both `gf -m s7 eval` and `gf eval-gf0` with identical stdout
;; except the last echo line (gf0 "(#<unspecified>)" vs s7 "#<unspecified>").
(begin
  (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  (display (fact 5)) (newline)
  (define counter
    (lambda ()
      (define n 0)
      (define bump (lambda () (set! n (+ n 1)) n))
      (bump) (bump)))
  (display (counter)) (newline)
  (display (let ((a 1) (b 2)) (+ a b))) (newline)
  (display (let* ((a 1) (b (+ a 1))) b)) (newline)
  (display (letrec ((e (lambda (n) (if (= n 0) 1 (o (- n 1)))))
                    (o (lambda (n) (if (= n 0) 0 (e (- n 1))))))
             (e 10))) (newline)
  (define my-map (lambda (f xs)
                   (if (null? xs) '() (cons (f (car xs)) (my-map f (cdr xs))))))
  (display (my-map (lambda (x) (* x 2)) '(1 2 3))) (newline)
  (display ((lambda args args) 1 2 3)) (newline)
  (display 'done) (newline))
