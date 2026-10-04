(import (scheme base) (scheme char) (liii check))
(check-set-mode! 'report-failed)

;; 6.10: multi-shot continuations preserve mutations and accept multiple values.
(let ((saved #f) (visits 0))
  (let-values (((a b) (call/cc (lambda (k) (set! saved k) (values 1 2)))))
    (set! visits (+ visits 1))
    (cond ((= visits 1) (check (list a b) => '(1 2)) (saved 3 4))
          ((= visits 2) (check (list a b) => '(3 4)) (saved 5 6))
          (else (check (list a b visits) => '(5 6 3))))))
(check (call-with-values (lambda () (call/cc (lambda (k) (k)))) list) => '())

;; Dynamic-wind transitions exit inner-to-outer and enter outer-to-inner.
(let ((saved #f) (resumed? #f) (log '()))
  (define (record x) (set! log (cons x log)))
  (dynamic-wind
    (lambda () (record 'outer-in))
    (lambda ()
      (dynamic-wind
        (lambda () (record 'inner-in))
        (lambda () (call/cc (lambda (k) (set! saved k) #f)))
        (lambda () (record 'inner-out))))
    (lambda () (record 'outer-out)))
  (if resumed?
    (check (reverse log)
           => '(outer-in inner-in inner-out outer-out
                outer-in inner-in inner-out outer-out))
    (begin (set! resumed? #t) (saved #t))))

;; Callback continuations cannot mutate results returned by an earlier visit.
(for-each
  (lambda (mapper)
    (let ((saved #f) (resumed? #f) (first #f))
      (let ((result (mapper (lambda (x)
                             (if (= x 1)
                               (call/cc (lambda (k) (set! saved k) x))
                               x)))))
        (if resumed?
          (begin (check first => '(1 2)) (check result => '(9 2)))
          (begin (set! first result) (set! resumed? #t) (saved 9))))))
  (list (lambda (f) (map f '(1 2)))
        (lambda (f) (vector->list (vector-map f #(1 2))))))

;; Keep the actual vector to detect mutation, rather than just a list snapshot.
(let ((saved #f) (resumed? #f) (first #f))
  (let ((result (vector-map (lambda (x)
                             (if (= x 1)
                               (call/cc (lambda (k) (set! saved k) x))
                               x))
                           #(1 2))))
    (if resumed?
      (begin (check first => #(1 2)) (check result => #(9 2)))
      (begin (set! first result) (set! resumed? #t) (saved 9)))))
(check (vector-map + #(1 2) #(10)) => #(11))
(check (vector-map + #() #(10 20)) => #())
(let ((seen '()))
  (vector-for-each (lambda (a b) (set! seen (cons (+ a b) seen)))
                   #(1 2 3) #(10 20))
  (check (reverse seen) => '(11 22)))

(let ((saved #f) (resumed? #f) (first #f))
  (let ((result (string-map (lambda (ch)
                             (if (char=? ch #\a)
                               (call/cc (lambda (k) (set! saved k) ch))
                               ch)) "ab")))
    (if resumed?
      (begin (check first => "ab") (check result => "zb"))
      (begin (set! first result) (set! resumed? #t) (saved #\z)))))

(check-report)
