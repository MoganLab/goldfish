(import (scheme base) (scheme case-lambda) (liii check))
(check-set-mode! 'report-failed)

;; 3.5: each source-level tail context must survive lowering to the machine loop.
(define (through-context n)
  (if (zero? n) 'done
    (case (modulo n 15)
      ((0) (begin #f (through-context (- n 1))))
      ((1) (cond ((positive? n) (through-context (- n 1))) (else #f)))
      ((2) (and #t (through-context (- n 1))))
      ((3) (or #f (through-context (- n 1))))
      ((4) (when #t (through-context (- n 1))))
      ((5) (unless #f (through-context (- n 1))))
      ((6) (let ((next (- n 1))) (through-context next)))
      ((7) (let* ((next (- n 1))) (through-context next)))
      ((8) (letrec ((next (lambda () (through-context (- n 1))))) (next)))
      ((9) (letrec* ((next (- n 1))) (through-context next)))
      ((10) (let-values (((next) (values (- n 1)))) (through-context next)))
      ((11) (let*-values (((next) (values (- n 1)))) (through-context next)))
      ((12) (let-syntax ((go (syntax-rules () ((_ x) (through-context x)))))
              (go (- n 1))))
      ((13) (letrec-syntax ((go (syntax-rules () ((_ x) (through-context x)))))
              (go (- n 1))))
      (else (do ((next (- n 1))) (#t (through-context next)))))))
(check (through-context 9000) => 'done)

(letrec ((even (case-lambda ((n) (if (zero? n) #t (odd (- n 1))))))
         (odd (lambda (n) (if (zero? n) #f (even (- n 1))))))
  (check (even 9000) => #t))
(letrec ((loop (lambda (n)
                 (if (zero? n) 'done
                   (cond ((- n 1) => loop))))))
  (check (loop 9000) => 'done))
(letrec ((loop (lambda (n)
                 (case n
                   ((0) 'done)
                   (else => (lambda (k) (loop (- k 1))))))))
  (check (loop 9000) => 'done))
(letrec ((loop (lambda (n)
                 (if (zero? n) 'done
                   (case (modulo n 2)
                     ((0 1) => (lambda (k) (loop (- n 1)))))))))
  (check (loop 9000) => 'done))
(letrec ((loop (lambda (n)
                 (if (zero? n) 'done
                   (apply loop (list (- n 1)))))))
  (check (loop 9000) => 'done))
(letrec ((loop (lambda (n)
                 (if (zero? n) 'done
                   (call-with-values (lambda () (- n 1)) loop)))))
  (check (loop 9000) => 'done))
(letrec ((loop (lambda (n)
                 (if (zero? n) 'done
                   (call/cc (lambda (k) (loop (- n 1))))))))
  (check (loop 9000) => 'done))

(check-report)
