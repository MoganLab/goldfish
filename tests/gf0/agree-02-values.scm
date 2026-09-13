;; agree-02: values protocol (non-divergent cases only).
(begin
  (display (call-with-values (lambda () (values 1 2)) list)) (newline)
  (display (call-with-values (lambda () (values)) list)) (newline)
  (display (call-with-values (lambda () 7) (lambda (v) v))) (newline)
  (display (call-with-values (lambda () (values 1 2 3))
                             (lambda (a b . r) (list a b r)))) (newline)
  (display (let-values (((x y) (values 10 20))) (+ x y))) (newline)
  (display (begin (values 4 5) 6)) (newline)
  (display 'done) (newline))
