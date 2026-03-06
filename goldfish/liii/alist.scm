;
; BSD License by Peter Danenberg
;

(define-library (liii alist)
  (import (liii base) (liii list) (liii error))
  (export alist? alist-cons alist-ref alist-ref/default vector->alist)
  (begin

    (define (alist? l)
      (and (list? l) (every pair? l)))

    (define alist-ref
      (lambda (alist key . rest)
        (let ((thunk (if (null? rest)
                         (lambda () (error 'alist-ref "key not found: ~s" key))
                         (car rest)))
              (= (cond ((null? rest) eqv?)
                       ((null? (cdr rest)) eqv?)
                       (else (cadr rest)))))
          (let ((value (assoc key alist =)))
            (if value (cdr value) (thunk))))))

    (define alist-ref/default
      (lambda (alist key . rest)
        (if (null? rest)
            (error 'alist-ref/default "missing default argument")
            (let ((default (car rest))
                  (= (if (null? (cdr rest)) eqv? (cadr rest))))
              (alist-ref alist key (lambda () default) =)))))

    ; MIT License
    ; Copyright guenchi (c) 2018 - 2019
    (define vector->alist
      (typed-lambda ((x vector?))
        (if (zero? (length x))
            '()
            (let loop ((x (vector->list x))
                       (n 0))
              (cons (cons n (car x)) (if (null? (cdr x)) '() (loop (cdr x) (+ n 1))))))))))

