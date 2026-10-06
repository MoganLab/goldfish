; Reader-only micro-benchmark, for profiling and timing.
; Run: GOLDFISH_CACHE_DIR=<warm> bin/gf bench/micro/reader.scm
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

(report "read-forms/scheme-char" 50
        (lambda () (read-file "goldfish/scheme/char.scm")))
(report "read-forms/srfi-175" 50
        (lambda () (read-file "goldfish/srfi/srfi-175.scm")))
(report "read-forms/compiler" 10
        (lambda () (read-file "goldfish/compiler.scm")))
