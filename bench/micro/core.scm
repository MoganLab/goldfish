; Stage micro-benchmarks: reader, serializer, evaluator, allocation.
; Run via bench/micro/run.sh (a warm cache is fine; these do not write).
;
; Output is TSV: name<TAB>total_ms<TAB>iters.

(define (now) (g_monotonic-nanosecond))
(define (elapsed-ms t) (quotient (- (now) t) 1000000))

(define (report name iters thunk)
  (thunk)
  (let ((t (now)))
    (let loop ((i 0))
      (when (< i iters)
        (thunk)
        (loop (+ i 1))))
    (display name)
    (display "\t")
    (display (elapsed-ms t))
    (display "\t")
    (display iters)
    (newline)))

(define (read-file path) (call-with-input-file path read-forms))

(report "read-forms/scheme-char" 20
        (lambda () (read-file "goldfish/scheme/char.scm")))
(report "read-forms/srfi-175" 20
        (lambda () (read-file "goldfish/srfi/srfi-175.scm")))
(report "read-forms/compiler" 5
        (lambda () (read-file "goldfish/compiler.scm")))

(define char-forms (read-file "goldfish/scheme/char.scm"))
(define serialize (module-ref the-expander-library 'serialize-cache-sexp))
(define deserialize (module-ref the-expander-library 'deserialize-cache-sexp))
(define serialized (serialize char-forms))
(report "serialize/char-forms" 20 (lambda () (serialize char-forms)))
(report "deserialize/char-forms" 20 (lambda () (deserialize serialized)))

(define (sum-loop n)
  (let loop ((i 0) (s 0)) (if (> i n) s (loop (+ i 1) (+ s i)))))
(report "eval/sum-1e6" 5 (lambda () (sum-loop 1000000)))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(report "eval/fib-25" 5 (lambda () (fib 25)))

(define (alloc-cons n)
  (let loop ((i 0) (a '())) (if (> i n) a (loop (+ i 1) (cons i a)))))
(report "alloc/cons-1e5" 5 (lambda () (alloc-cons 100000)))

(define (alloc-vector n)
  (let loop ((i 0))
    (when (< i n) (make-vector 100 i) (loop (+ i 1)))))
(report "alloc/vector-1e5" 5 (lambda () (alloc-vector 100000)))
