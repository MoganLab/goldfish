;; Read-only algorithm probe using the current source definition.
(define calls 0)
(define (%s7-ht-hash ht key) (set! calls (+ calls 1)) key)
(call-with-input-file "goldfish/expander/lib/native-hash-adapter.scm"
  (lambda (p)
    (let loop ((form (read p)))
      (cond
        ((eof-object? form) (error "resize definition missing"))
        ((and (pair? form) (eq? (car form) 'define) (pair? (cadr form))
              (eq? (caadr form) '%s7-ht-resize!))
         (eval form (interaction-environment)))
        (else (loop (read p)))))))
(define old (make-vector 8 '()))
(do ((i 0 (+ i 1))) ((= i 8))
  (vector-set! old i (list (cons i i))))
(define ht (vector 's7-hash-table old #f #f 8))
(%s7-ht-resize! ht 16)
(define entries
  (let loop ((i 0) (n 0))
    (if (= i 16) n
        (loop (+ i 1) (+ n (length (vector-ref (vector-ref ht 1) i)))))))
(write (list 'original-entries 8 'new-entries entries 'hash-calls calls))
(newline)
